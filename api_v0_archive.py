from global_func import QueryHandler, S3Handler
from notification_adapter import NotificationAdapter
from tornado.log import enable_pretty_logging
from tornado.options import options
import ast
import datetime
import dateutil.relativedelta
import magic
import settings
from tornado.web import MissingArgumentError
import time
import uuid
import tornado.escape
import tornado.ioloop
import tornado.web
import random
import tornado
import requests
import os
import facebook
import json
import ConfigParser
import base64
from requests_toolbelt import MultipartDecoder, MultipartEncoder

import base64
import urllib
from custom_error import BadAuthentication
config = ConfigParser.ConfigParser()
config.read('config.py')


def check_udid_and_apk_version(request_handler_object):
    request_handler_object.get_argument('apk_version')
    request_handler_object.get_argument('udid')


class SetLocationHandler(tornado.web.RequestHandler):
    """
    This class handles the storage of locations of a user in the server
    For web interfacing it implements two methods i.e get and post, which 
    corresponds to the tornado framework requirements. 
    Methods : 
        get :
            :params 
                user username  
                lng  longtitude
                lat  latitude
            :response 
                :success => {'status':settings.STATUS_200, 'info': 'Success'}
                :failure => {'status': 500, 'info': 'Error [Error message]'}

    """

    def get(self):
        try:
            response = {}
            check_udid_and_apk_version(self)
            username = self.get_query_argument("user")
            longtitude = self.get_query_argument("lng")
            latitude = self.get_query_argument("lat")
            query = " UPDATE users SET lat = %s, lng = %s " \
                    " WHERE username = %s; "
            QueryHandler.execute(query, (latitude, longtitude, username))
        except MissingArgumentError, status:
            response["info"] = status.log_message 
            response["status"] = settings.STATUS_400
        except Exception, e:
            response["info"] = " Error %s " % e
            response["status"] = settings.STATUS_500
        else:
            response['message'] = settings.SUCCESS_RESPONSE
            response["status"] = settings.STATUS_200
        finally:
            self.write(response)


class User:
    """
    This class defines the user object which can be used for successive operations
    to be done on the user object.
    Attributes to be passed on the initialization:
        username -- username of the user most typically the full jid of the user 
        password [optional] -- optionally if the user has been created.
    Methods: 
        authenticate -- authenticates the user
        handle_creation -- handles the creation of user
        _is_token_correct -- checks whether the otp token given by the user is correct/expired
        _create_new -- creates a new user after verifying the authenticity of the user
        handle_registration -- starts the registration process for the user
        _delete_registered -- deletes the registered user so that new otp can be sent to the user
        _register -- registers the user
        _send_message -- send the otp token to the users phone number
    """
    def __init__(self, phone_number = None, password = None, username = None):
        self.username = username
        self.password = password
        self.phone_number = phone_number

    def authenticate(self):
        """
        In case of already created user, this functions authicates the user. 
        return true if authenticated
        return false if not authenticated
        """
        query = " SELECT * FROM users WHERE username = %s AND password = %s; "
        variables = (self.username, self.password,)

        record = QueryHandler.get_results(query, variables)

        if not record:
            raise BadAuthentication

    def handle_creation(self, auth_code):
        """
        Handles the creation aspect of the User class
        Parameters:- 
            auth_code - The otp token sent to the users cellphone
        Response :-
            If token correct:
                "Success",settings.STATUS_200, [password]
            If user already created
                " User already created",settings.STATUS_200, [password]
            If wrong or expired auth token
                " Wrong or Expired Token ",settings.STATUS_400, None
        """
        try:
            if self._is_token_correct(auth_code):
                query = " SELECT * FROM users WHERE phone_number = %s ;"
                variables = (self.phone_number, )
                record = QueryHandler.get_results(query, variables)
                
                if record:
                    self.username = record[0]['username']
                    response, status = self._reset_password()
                else:
                    response, status = self._create_new()
            else:
                response, status = " Wrong or Expired Token ", settings.STATUS_400
            pass
        except Exception, e:
            response, status = " Error %e " % e, settings.STATUS_500
        finally:
            self._delete_registered()
            return response, status, self.password, self.username

    def _generate_username(self):
        self.username = self._generate_random()
    
    def _generate_password(self):
        self.password = self._generate_random()

    def _generate_random(self, n = 10):
        return (uuid.uuid4().hex)[:n]

    def _reset_password(self):
        """
            This functions resets the password of a user
            Response:-
                If successfully
                    Response, Status = "Success",settings.STATUS_200
                Else
                    Response, Status = "Error [Error]", 500
        """
        try: 
            self._generate_password()
            query = " UPDATE users SET password = %s WHERE username = %s; " 
            variables = (self.password, self.username)
            QueryHandler.execute(query, variables)
            response, status = "Success", settings.STATUS_200
        except Exception, e:
            response, status = " Error %e " % e, settings.STATUS_500
        finally:
            return response, status


    def _is_token_correct(self, auth_code):
        """
        Authenticates the otp token sent to the user
        Parameters :-
            auth_code -- The otp token sent to the user
        Response :-
            True if correct
            False if wrong token
        """
        query = " SELECT * FROM registered_users WHERE phone_number = %s AND authorization_code = %s ;"
        variables = (self.phone_number, auth_code,)

        record = QueryHandler.get_results(query, variables)
        try:
            if record and (record[0]['expiration_time'] > int(time.time())):
                is_token_correct = True
            else:
                is_token_correct = False
        except:
            is_token_correct = False
        finally:
            return is_token_correct

    def _create_new(self):
        """
        Creates a new user in the database.
        """
        try:
            while True:
                self._generate_username()
                self._generate_password()

                query = " SELECT * FROM users WHERE username = %s;"
                variables = (self.username,)
                record = QueryHandler.get_results(query, variables)

                if not record:
                    query = " INSERT INTO users (username, phone_number, password) VALUES "\
                    + "(%s, %s, %s);"
                    variables = (self.username, self.phone_number, self.password)
                    QueryHandler.execute(query, variables)
                    break
            response, status = "Success", settings.STATUS_200
        except Exception, e:
            response, status = " %s " % e, settings.STATUS_500
        finally:
            return response, status

    def handle_registration(self):
        """
        Handles the registration of the user in the database
        """
        self._delete_registered()
        response, status = self._register()
        return response, status

    def _delete_registered(self):
        """
        Deletes the registered users in the database, which have been created or 
        when new otp token has to be sent.
        """
        query = " DELETE FROM registered_users WHERE phone_number = %s ;"
        variables = (self.phone_number,)
        QueryHandler.execute(query, variables)

    def _register(self):
        """
        Registers the users in the database
        """
        random_integer = random.randint(1000,9999)
        expiration_time = int(time.time()) + int(config.get('registration', 'expiry_period_sec'))

        query = " INSERT INTO registered_users (phone_number, authorization_code, expiration_time) VALUES ( %s, %s, %s); "
        variables = (self.phone_number, random_integer, expiration_time)
        try:
            QueryHandler.execute(query, variables)
            return self._send_message(random_integer)
        except Exception, e:
            return " Error while sending message : % s" % e, settings.STATUS_500

    def _send_message(self, random_integer):
        """
        Sends the otp token to the user
        """
        number = self.phone_number
        message = config.get('database','message') + "  " + str(random_integer)
        payload = {
            'method': 'SendMessage',
            'send_to': str.strip(number),
            'msg': str.strip(message),
            'msg_type': 'TEXT',
            'userid': config.get('database','gupshup_id'),
            'auth_scheme': 'plain',
            'password': config.get('database','gupshup_password'),
            'v': '1.1',
            'format': 'text',
        }
        response = requests.get(config.get('database','message_gateway'), params=payload)
        response = str.split(str(response.text),'|')
        if str.strip(str.lower(response[0])) == "success":
            return "Success",settings.STATUS_200
        else:
            error = response[2]
            return error, 500


class FacebookHandler(tornado.web.RequestHandler):
    """
    Stores the facebook id of the user and finds his friends from the stored ids
    Methods:
        get - Implements the http get method for tornado frameowrk and also sets the user
            facebook details in the database
            Parameters:
                fb_id -- fb_id of the user
                token -- fb token of the user
                user_id -- xmpp user_id 
            Response: 
                {'info' : 'Error [Error]', 'status': 500}
                {'info' : [{'id': [friend_id], 'fb_name': [friend name]} .. ], 'status': 500}
        _get_friends_id - 
            Gets friends facebook id. 
        _set_fb_details - 
            sets the users fb details in the database
    """
    def get(self):
        try:
            check_udid_and_apk_version(self)
            response = {}
            fb_id = str(self.get_query_argument("fb_id"))
            token = str(self.get_query_argument("token"))
            user_id = str(self.get_query_argument('id'))

            username = str.split(user_id, "@")[0]

            args = {'fields': 'id,name,email,friends', }
            graph = facebook.GraphAPI(token)
            fb_json = graph.get_object('me', **args)

            fb_name = fb_json['name']

            self._set_fb_details(fb_id, username, fb_name)

            friends_details = self._get_friends_id(fb_json['friends']['data'])
        # TO-DO explicit error messages
        except MissingArgumentError, status:
            response["info"] = status.log_message 
            response["status"] = settings.STATUS_400
        except Exception, e:
            response['info'] = " Error : % s " % e
            response['status'] = settings.STATUS_500
        else:
            if not response:
                response['list'] = friends_details
                response['status'] = settings.STATUS_200
        finally:
            self.write(response)

    def _get_friends_id(self, friends_details):
        friends_id_name = []
        for friend_detail in friends_details:
            friend_id_name = {}
            query = " SELECT * FROM users WHERE fb_id = %s;"
            variables = (friend_detail['id'],)
            results = QueryHandler.get_results(query, variables)
            if len(results) > 0:
                friend_id_name['id'] = results['username'] + '@mm.io'
                friend_id_name['fb_name'] = results['fb_name'] + '@mm.io'
                friends_id_name.append(friend_id_name)
        return friends_id_name

    def _set_fb_details(self, fb_id, username, fb_name):
        query = " UPDATE users SET fb_id = %s, fb_name = %s WHERE username = %s ;"
        variables = (fb_id, fb_name, username)
        QueryHandler.execute(query, variables)


class RegistrationHandler(tornado.web.RequestHandler):
    """
    Handles the registration of the user.
    Parameters:- 
        phone_number -- phone number of the user to be registered
    Response:-
        {'info': 'Success', 'status':settings.STATUS_200} if successfully registered 
        {'info': 'Error [Error]', 'status': 500} if not successfully registered 
    """
    def get(self):
        response = {}
        try:
            check_udid_and_apk_version(self)
            phone_number = str(self.get_query_argument("phone_number"))
            user = User(phone_number)
            response['info'], response['status'] = user.handle_registration()
        except MissingArgumentError, status:
            response["info"] = status.log_message 
            response["status"] =settings.STATUS_400   
        except Exception, e:
            response['info'] = " Error: %s " % e
            response['status'] = settings.STATUS_500
        finally:
            self.write(response)


class CreationHandler(tornado.web.RequestHandler):
    """
    Handles the creation of the user.
    Query Parameters:- 
        phone_number -- phone number of the user to be registered
        auth_code -- auth code otp sent to the user
    Response:-
        {'info': 'Success', 'status': 200
        , 'username': [username], 'password': [password]} if successfully registered 
        {'info': 'Wrong or Expired Token', 'status': 400
        , 'username': null, 'password': null} if wrong auth code
        {'info': 'Error [Error]', 'status': 500} if not successfully registered 
        {'info': 'Error [Error]', 'status': 500} if not successfully registered 
    """
    def get(self):
        response = {}
        try:
            check_udid_and_apk_version(self)
            phone_number = str(self.get_query_argument("phone_number"))
            auth_code = str(self.get_query_argument("auth_code"))
            user = User(phone_number)
            response['info'], response['status'], response['password'], response['username'] = user.handle_creation(auth_code)
        except MissingArgumentError, status:
            response["info"] = status.log_message 
            response["status"] = settings.STATUS_400
        except Exception, e:
            response['info'] = " Error %s " % e
            response['status'] = settings.STATUS_500
        finally:
            self.write(response)

class MediaPresentHandler(tornado.web.RequestHandler):
    def get(self):
        check_udid_and_apk_version(self)
        response = {}
        try:
            file_name = "media/" + self.get_query_argument("name")
            if os.path.isfile(file_name):
                response['info'] = 'Present'
                response['status'] =settings.STATUS_200
            else:
                response['info'] = 'Not Found'
                response['status'] =settings.STATUS_400
        except MissingArgumentError, status:
            response["info"] = status.log_message 
            response["status"] =settings.STATUS_400
        except Exception, e:
            response['status'] = settings.STATUS_500
            response['info'] = 'error is: %s' % e
        finally:
            self.write(response)


@tornado.web.stream_request_body
class MediaHandler(tornado.web.RequestHandler):
    response = {}

    def prepare(self):
        self.file_content = ''

    def post(self):
        response = {}
        try:
            check_udid_and_apk_version(self)
            file_name = self.request.headers['Checksum']
            file_name = "media/" + file_name
            if not os.path.isfile(file_name):
                media_file = open(file_name, 'w')
                media_file.write(self.file_content)
                media_file.flush()
            response['status'] =settings.STATUS_200
            response['info'] = 'Success'
        except MissingArgumentError, status:
            response["info"] = status.log_message 
            response["status"] =settings.STATUS_400
        except Exception, e:
            response['status'] = settings.STATUS_500
            response['info'] = 'error is: %s' % e
        finally:
            self.write(response)

    def get(self):
        try:
            response = {}
            check_udid_and_apk_version(self)
            file_name = "media/" + self.get_arguments("name")[0]
            if os.path.isfile(file_name):
                with open(file_name, 'r') as file_content:
                    file_size = 0
                    while 1:
                        data = file_content.read(16384) # or some other nice-sized chunk
                        if not data:
                            break
                        file_size += len(data)
                        self.write(data)
                    self.request.headers['Content-Length', file_size]
                    self.add_header('Content-Length', file_size)
                    file_content.close()
                    self.finish()
            else:
                response['info'] = 'Not Found'
                response['status'] = settings.STATUS_400
                self.write(response)
        except MissingArgumentError, status:
            response["info"] = status.log_message 
            response["status"] =settings.STATUS_400
        except Exception, e:
            response['status'] = settings.STATUS_500
            response['info'] = 'error is: %s' % e
            self.write(response)

    def data_received(self, data):
        self.file_content += data


class IOSMediaHandler(tornado.web.RequestHandler):

    def data_validation(self, headers, body):
        response = {'info': '', 'status': 0}

        # 'Content-Type' not present in the header
        if not headers.get('Content-Type'):
            response['info'] = " Bad Request: 'Content-Type' field not present in the Header!"
            response['status'] = settings.STATUS_400
            return response

        # 'Checksum' not present in the header
        if response['status'] == 0 and not headers.get('Checksum'):
            response['info'] = " Bad Request: 'Checksum' field not present in the Header!"
            response['status'] = settings.STATUS_400
            return response

        # body not present
        if response['status'] == 0 and not body:
            response['info'] = " Bad request: Request body not present!"
            response['status'] = settings.STATUS_400
        return response

    def post(self):
        response = {}
        try:
            check_udid_and_apk_version(self)
            headers = self.request.headers
            body = self.request.body

            # data validation
            response = self.data_validation(headers, body)

            if response['status'] != settings.STATUS_400:
                decoder = MultipartDecoder(body, content_type=headers.get('Content-Type'))
                file_content = decoder.parts[0].content
                file_name = "media/" + headers.get('Checksum')
                if os.path.isfile(file_name):
                    response['status'] = settings.STATUS_422
                    response['info'] = "Error: File with same name already exists!"
                else:
                    media_file = open(file_name, 'w')
                    media_file.write(file_content)
                    media_file.flush()
                    response['status'] = settings.STATUS_200
                    response['info'] = settings.SUCCESS_RESPONSE
        except MissingArgumentError, status:
            response["info"] = status.log_message 
            response["status"] =settings.STATUS_400
        except Exception as e:
            response['status'] = settings.STATUS_500
            response['info'] = " Error is: %s" % e
        finally:
            self.write(response)


class GetNearbyUsers(tornado.web.RequestHandler):
    def get(self):
        response = {}
        try:
            check_udid_and_apk_version(self)
            self.radius = self.get_query_argument('radius')
            self.lat = self.get_query_argument('lat')
            self.lng = self.get_query_argument('lng')
            users = self.get_nearby_users()
            response['status'] =settings.STATUS_200
            response['info'] = 'Success'
            response['users'] = users
        except MissingArgumentError, status:
            response["info"] = status.log_message 
            response["status"] =settings.STATUS_400
        except Exception, e:
            response['status'] = settings.STATUS_500
            response['info'] = 'Error: %s' % e
        finally:
            self.write(response)

    def get_nearby_users(self):
        query = "SELECT users.username, earth_distance(ll_to_earth(%s, %s),"\
            + " ll_to_earth(users.lat, users.lng)) as distance, users.lat AS lat, users.lng AS lng "\
            + ", string_agg(interest.interest_name, ',') as interests "\
            + " FROM users  "\
            + " left outer join users_interest on (users.username = users_interest.username) "\
            + " left outer join interest on (users_interest.interest_id = interest.interest_id)"\
            + " WHERE earth_box(ll_to_earth(%s, %s),  %s) @> ll_to_earth(users.lat, users.lng) AND  "\
            + " is_available = True "\
            + " GROUP BY users.username ORDER BY distance ASC;"
        variables = (self.lat, self.lng, self.lat, self.lng, self.radius)
        records = QueryHandler.get_results(query, variables)
        return records


class UserInterestHandler(tornado.web.RequestHandler):
    """
    This class creates a link between users and interests. The interests have to
    stored beforehand.

    Methods : 
        get :
            :params 
                username => username 
                interests => list of interests like interests=football&interests=cricket 
            :response 
                :success => {'status':settings.STATUS_200, 'info': 'Success'}
                :failure => {'status': 500, 'info': 'Error [Error message]'}     
    """
    def get(self):
        response = {}
        try:
            check_udid_and_apk_version(self)
            username = self.get_query_argument('username')
            interests = self.request.arguments['interests']
            interests = map(lambda interest: interest.lower(), interests)

            query = " DELETE FROM users_interest WHERE username = %s;"
            variables = (username,)
            QueryHandler.execute(query, variables)            

            query = "INSERT INTO users_interest (interest_id, username) "\
                " (SELECT interest_id, %s FROM interest WHERE "\
                + " OR ".join(map( lambda interest: "interest_name = '" + interest + "'" , interests))\
                + ");"         
            variables = (username, )
            QueryHandler.execute(query, variables)
            response['status'] =settings.STATUS_200
            response['info'] = "Success"
        except MissingArgumentError, status:
            response["info"] = status.log_message 
            response["status"] =settings.STATUS_400
        except Exception, e:
            response['status'] = settings.STATUS_500
            response['info'] = "Error: %s " % e
        finally:
            self.write(response)


class IOSSetUserDeviceId(tornado.web.RequestHandler):

    def post(self):
        response = {}
        try:
            check_udid_and_apk_version(self)
            username = str(self.get_body_argument('user'))
            password = str(self.get_body_argument('password'))
            udid = str(self.get_body_argument('token'))

            user = User(password = password, username = username)
            user.authenticate()
            query = "UPDATE users SET apple_udid=%s WHERE username=%s;"
            variables = (udid, username)
            QueryHandler.execute(query, variables)

            response['info'] = "Success"
            response['status'] =settings.STATUS_200
        except BadAuthentication, status:
            response["info"] = status.log_message 
            response["status"] =settings.STATUS_400
        except MissingArgumentError, status:
            response["info"] = status.log_message 
            response["status"] =settings.STATUS_400
        except Exception as e:
            response['info'] = "Error: %s" % e
            response['status'] = settings.STATUS_500
        finally:
            self.write(response)


class FootballEvents(tornado.web.RequestHandler):
    def post(self):
        event = tornado.escape.json_decode(self.request.body)
        if event:
            NotificationAdapter(event, "Football").notify()


class TennisEvents(tornado.web.RequestHandler):
    def post(self):
        event = tornado.escape.json_decode(self.request.body)
        if event:
            NotificationAdapter(event, "Tennis").notify()


class CricketEvents(tornado.web.RequestHandler):
    def post(self):
        event = tornado.escape.json_decode(self.request.body)
        if event:
            NotificationAdapter(event, "Cricket").notify()


class ContactJidsHandler(tornado.web.RequestHandler):
    """
    This class handles the retrival of jids in the contact list
    of the user
    Methods : 
        get :
            :params 
                username => username
                password => password
                apk_version and udid
                contacts => a list of all the contacts in the users phone                 
            :response 
                :success => {'status':200, 'info': 'Success', 'jids': [List of jids]}
                :failure => {'status': 404, 'info': ' Bad Authentication Info'} in case of bad authentication          
                :failure => {'status': 400, 'info': ' MissingArgumentError'} in case of missing arguments          
                :failure => {'status': 500, 'info': 'Error [Error message]'} in case of internal server error
    """

    def get_contacts_jids(self, username, contacts):
        where_arguments = [" phone_number = %s "] * len(contacts)
        contacts = tuple(contacts)
        query =  " SELECT username FROM users WHERE " + " OR ".join(where_arguments)
        records = QueryHandler.get_results(query, (contacts))
        return map(lambda x: x['username'], records)


    def post(self):
        response = {}
        try:
            username = self.get_argument('username')
            password = self.get_argument('password')
            contacts = self.get_arguments('contacts') 
            assert type(contacts) == list 
            contacts = map(lambda x: str(x), contacts)
            user = User(username = username, password = password)
            user.authenticate()
            check_udid_and_apk_version(self)
            response['info'] = 'Success'
            response['status'] = settings.STATUS_200
            response['jids'] = self.get_contacts_jids(username, contacts)
        except BadAuthentication, status:
            response["info"] = status.log_message 
            response["status"] = settings.STATUS_404
        except MissingArgumentError, status:
            response["info"] = status.log_message 
            response["status"] = settings.STATUS_400
        except AssertionError:
            response["info"] = "Bad contacts value type" 
            response["status"] = settings.STATUS_400
        except Exception as e:
            response['info'] = "Error: %s" % e
            response['status'] = settings.STATUS_500
        finally:
            self.write(response)


class NearbyUsersWithSameInterests(tornado.web.RequestHandler):
    nearby_users_dict = {}
    user_interests = []

    def get_recently_online_anonymous_users(self):
        current_dt = datetime.datetime.now()
        for user in self.nearby_users_dict.keys():
            query = "SELECT last_seen FROM users WHERE username=%s;"
            variables = (user,)
            last_seen = QueryHandler.get_results(query, variables)[0]['last_seen']
            user_dt = datetime.datetime.fromtimestamp(float(last_seen))
            relativde_td = dateutil.relativedelta.relativedelta (current_dt, user_dt)
            if not ((relativde_td.years, relativde_td.months, relativde_td.days) == (0,0,0) and relativde_td.hours <= 1):
                self.nearby_users_dict.pop(user)

    def get_banned_users(self):
        query = "SELECT VALUE FROM privacy_list_data, privacy_list WHERE privacy_list_data.id=privacy_list.id " \
                "AND privacy_list.username=%s;"
        variables = (self.username,)
        banned_users = QueryHandler.get_results(query, variables)
        return banned_users

    def get_nearby_friends_with_same_interests(self):
        nearby_friends = {}
        query = "SELECT jid FROM rosterusers WHERE username=%s;"
        result = QueryHandler.get_results(query, (self.username,))
        user_friends = [friend['jid'].split('@')[0] for friend in result] if result else []
        nearby = list(set(self.nearby_users_dict.keys()).intersection(user_friends))

        for friend in nearby:
            common_interests = list(set(self.nearby_users_dict.pop(friend)).intersection(self.user_interests))
            if common_interests:
                nearby_friends[friend] = common_interests
        return nearby_friends

    def get_nearby_anonymous_users_with_same_interests(self):
        nearby_anonymous_users = {}
        for banned in self.get_banned_users():
            self.nearby_users_dict.pop(banned['value'].split('@')[0])
        self.get_recently_online_anonymous_users()

        for user, interests in self.nearby_users_dict.items():
            common_interests = list(set(interests).intersection(self.user_interests))
            if common_interests:
                nearby_anonymous_users[user] = common_interests
        return nearby_anonymous_users

    def get(self):
        response = {}
        try:
            check_udid_and_apk_version(self)
            self.username = str(self.get_argument('username'))
            nearby_users = ast.literal_eval(self.get_argument('nearby_users'))
            if nearby_users:
                for user in nearby_users:
                    if user['interests'] != None:
                        self.nearby_users_dict[user['username']] = user['interests'].split(',')

                self.user_interests = self.nearby_users_dict.pop(self.username)     # fetch user's interests
                if self.user_interests != ['']:
                    nearby_friends = self.get_nearby_friends_with_same_interests()      # get friends with similar interests
                    nearby_anonymous_users = self.get_nearby_anonymous_users_with_same_interests()     # get anonymous users with similar interests

                    response['result'] = {'friends': nearby_friends, 'anonymous': nearby_anonymous_users}
                    response['info'] = settings.SUCCESS_RESPONSE
                    response['status'] = settings.STATUS_200
                else:
                    response['info'] = "Bad Request: This user has NO interests!"
                    response['status'] = settings.STATUS_400
            else:
                response['info'] = "Bad Request: User has no users nearby!"
                response['status'] = settings.STATUS_400
        except BadAuthentication, status:
            response["info"] = status.log_message
            response["status"] = settings.STATUS_400
        except MissingArgumentError, status:
            response["info"] = status.log_message
            response["status"] = settings.STATUS_400
        except Exception as e:
            response['info'] = "Error: %s" % e
            response['status'] = settings.STATUS_500
        finally:
            self.write(response)


def make_app():
    return tornado.web.Application([
                                       (r"/register", RegistrationHandler),
                                       (r"/create", CreationHandler),
                                       (r"/set_location", SetLocationHandler),
                                       (r"/retrieve_nearby_users", GetNearbyUsers),
                                       (r"/fb_friends", FacebookHandler),
                                       (r"/football_notifications", FootballEvents),
                                       (r"/tennis_notifications", TennisEvents),
                                       (r"/media", MediaHandler),
                                       (r"/media_present", MediaPresentHandler),
                                       (r"/media_multipart", IOSMediaHandler),
                                       (r"/cricket_notifications", CricketEvents),
                                       (r"/set_user_interests", UserInterestHandler),
                                       (r"/set_udid", IOSSetUserDeviceId),
                                       (r"/get_contact_jids", ContactJidsHandler),
                                       (r"/get_nearby_similar_interests_users", NearbyUsersWithSameInterests),
                                       ],
                                   autoreload = True,
                                   )

if __name__ == "__main__":
    app = make_app()
    options.log_file_prefix  = "tornado_log"
    enable_pretty_logging(options=options)
    app.listen(int(config.get('tornado', 'listening_port')))
    tornado.ioloop.IOLoop.current().start()
