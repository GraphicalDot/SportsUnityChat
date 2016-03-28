from global_func import QueryHandler, S3Handler, merge_dicts
from notification_adapter import NotificationAdapter
from tornado.log import enable_pretty_logging
from tornado.options import options
import magic
import settings
from tornado.web import MissingArgumentError
from psycopg2 import IntegrityError
import utils
import base64
import ConfigParser
import facebook
import json
import os
import random
import requests
import time
import tornado
import tornado.ioloop
import tornado.autoreload
import tornado.escape
import tornado.web
import uuid
from requests_toolbelt import MultipartDecoder
from custom_error import BadAuthentication, BadInfoSuppliedError
import admin_api
from notification_handler import NotificationHandler
config = ConfigParser.ConfigParser()
config.read('config.py')

def merge_body_arguments(request_handler_object):
    try:
        body_argument = json.loads(request_handler_object.request.body)
        listing_function = lambda x: x if type(x) == list else [str(x)]
        body_argument = {k: listing_function(v) for k, v in body_argument.iteritems()}
        request_handler_object.request.arguments.update(body_argument)
        return request_handler_object.request.arguments
    except ValueError:
        return request_handler_object.request.arguments

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
            username = self.get_argument("user")
            longtitude = self.get_argument("lng")
            latitude = self.get_argument("lat")
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

    def check_if_user_blocked(self):
        query = "SELECT is_banned FROM users WHERE phone_number=%s AND is_banned=True;"
        variables = (self.phone_number,)
        return True if QueryHandler.get_results(query, variables) else False

    def handle_registration(self):
        """
        Handles the registration of the user in the database
        """
        self._delete_registered()
        response, status = (settings.USER_FORBIDDEN_ERROR, settings.STATUS_403) if self.check_if_user_blocked() \
            else self._register()
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

        try:
            status_info, status_code, gateway_response = utils.send_message(number=self.phone_number, message=settings.OTP_MESSAGE.format(random_integer))
            query = " INSERT INTO registered_users (phone_number, authorization_code, expiration_time, gateway_response) VALUES ( %s, %s, %s, %s); "
            variables = (self.phone_number, random_integer, expiration_time, str(gateway_response))
            QueryHandler.execute(query, variables)
            return status_info, status_code
        except Exception, e:
            return " Error while sending message : % s" % e, settings.STATUS_500


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
            fb_id = str(self.get_argument("fb_id"))
            token = str(self.get_argument("token"))
            user_id = str(self.get_argument('id'))

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
            phone_number = str(self.get_argument("phone_number"))
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
            phone_number = str(self.get_argument("phone_number"))
            auth_code = str(self.get_argument("auth_code"))
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
            file_name = "media/" + self.get_argument("name")
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
            file_name = "media/" + self.request.headers['Checksum']
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
            file_name = "media/" + self.get_argument("name")
            if os.path.isfile(file_name):
                with open(file_name, 'r') as file_content:
                    while 1:
                        data = file_content.read(16384) # or some other nice-sized chunk
                        if not data:
                            break
                        self.write(data)
                    file_content.close()
                    self.finish()
            else:
                response['info'] = 'Not Found'
                response['status'] = settings.STATUS_400
                self.write(response)
        except MissingArgumentError, status:
            response["info"] = status.log_message 
            response["status"] = settings.STATUS_400
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
    def get_user_interests(self):
        query = " SELECT interest_id FROM users_interest WHERE username = %s;"
        variables = (self.username,)
        return QueryHandler.get_results(query, variables)

    def insert_user_interest(self, new_interests):
        query = "INSERT INTO users_interest (interest_id, username) "\
            " (SELECT interest_id, %s FROM interest WHERE "\
            + " OR ".join(map( lambda interest: "interest_id = '" + interest + "'" , new_interests))\
            + ");"         
        variables = (self.username, )
        QueryHandler.execute(query, variables)

    def delete_user_interest(self, interests):
        query = " DELETE FROM users_interest WHERE "\
        +   "username = %s AND ( " + " OR ".join(["interest_id = %s "]*len(interests)) + " );"
        variables = [self.username] + interests
        QueryHandler.execute(query, variables)

    def post(self):
        response = {}
        try:
            self.request.arguments = merge_body_arguments(self)
            check_udid_and_apk_version(self)
            self.username = self.get_argument('username')
            self.password = self.get_argument('password')
            interests = self.request.arguments['interests']
            user = User(username = self.username, password = self.password)
            user.authenticate()

            interests_record = self.get_user_interests()
            new_interests = [interest for interest in interests if interest not in map(lambda x: x['interest_id'], interests_record)] 
            self.insert_user_interest(new_interests)
            response['status'] = settings.STATUS_200
            response['info'] = "Success"
        except BadAuthentication, status:
            response["info"] = status.log_message 
            response["status"] =settings.STATUS_400
        except MissingArgumentError, status:
            response["info"] = status.log_message 
            response["status"] =settings.STATUS_400
        except Exception, e:
            response['status'] = settings.STATUS_500
            response['info'] = "Error: %s " % e
        finally:
            self.write(response)

    def delete(self):
        response = {}
        try:
            self.request.arguments = merge_body_arguments(self)
            check_udid_and_apk_version(self)
            self.username = self.get_argument('username')
            self.password = self.get_argument('password')
            user = User(username = self.username, password = self.password)
            user.authenticate()
            interests = self.request.arguments['interests']

            self.delete_user_interest(interests)

            response['status'] =settings.STATUS_200
            response['info'] = "Success"
        except BadAuthentication, status:
            response["info"] = status.log_message 
            response["status"] =settings.STATUS_400
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
            username = str(self.get_argument('user'))
            password = str(self.get_argument('password'))
            udid = str(self.get_argument('token'))

            user = User(password = password, username = username)
            user.authenticate()
            query = "UPDATE users SET apple_token=%s WHERE username=%s;"
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


class SendAppInvitation(tornado.web.RequestHandler):
    """
    Sends invitation invite from app user to any of its contacts.
    """

    def data_validation(self, app_user, invited_user):
        query = "SELECT * FROM users WHERE username=%s;"
        variables = (app_user,)
        result = QueryHandler.get_results(query, variables)
        if not result:
            return ("Bad Request: User is not Registered!", settings.STATUS_400)

        query = "SELECT * FROM users WHERE phone_number=%s;"
        variables = (invited_user,)
        return ("Bad Request: Invited User is Already Registered!", settings.STATUS_400) \
            if QueryHandler.get_results(query, variables) else ('Valid', settings.STATUS_200)

    def post(self):
        response = {}
        try:
            app_user = str(self.get_argument('user'))
            invited_user = str(self.get_argument('invited_user')).strip()

            # data validation
            response['info'], response['status'] = self.data_validation(app_user, invited_user)

            if response['status'] not in settings.STATUS_ERROR_LIST:
                message = settings.APP_INVITATION_MESSAGE.format(app_user)
                response['info'], response['status'] = utils.send_message(invited_user, message)

        except MissingArgumentError, status:
            response['info'] = status.log_message
            response['status'] = settings.STATUS_400
        except Exception as e:
            response['info'] = 'Error: %s' % e
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
        query =  " SELECT username, phone_number FROM users WHERE " + " OR ".join(where_arguments)
        records = QueryHandler.get_results(query, (contacts))
        return records


    def post(self):
        response = {}
        self.request.arguments = merge_body_arguments(self)
        try:
            check_udid_and_apk_version(self)
            username = self.get_argument('username')
            password = self.get_argument('password')
            contacts = self.get_arguments('contacts')
            assert type(contacts) == list
            contacts = map(lambda x: str(x), contacts)
            user = User(username = username, password = password)
            user.authenticate()
            response['info'] = 'Success'
            response['status'] = settings.STATUS_200
            if len(contacts) == 0:
                response['jids'] = []
            else:
                response['jids'] = self.get_contacts_jids(username, contacts)
        except BadAuthentication, status:
            response["info"] = status.log_message
            response["status"] = settings.STATUS_404
        except KeyError, status:
            response["info"] = " Missing %e" % status.message
            response["status"] = settings.STATUS_400
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


class GetNearbyUsers(tornado.web.RequestHandler):

    def get_nearby_users(self):
        query = "WITH uinterest AS "\
            + "      ( "\
            + "            SELECT array_agg(interest.interest_name) AS uinterest FROM interest, users_interest  "\
            + "            WHERE users_interest.username = %s AND users_interest.interest_id = interest.interest_id "\
            + "      ),"\
            + " banned_users AS "\
            + "      ("\
            + "            SELECT split_part(privacy_list_data.value, '@', 1) AS bjids "\
            + "            FROM privacy_list_data, privacy_list "\
            + "            WHERE privacy_list.username = %s "\
            + "                  AND privacy_list.id = privacy_list_data.id "\
            + "                  AND privacy_list_data.action = 'd' "\
            + "                  AND privacy_list_data.value IS NOT NULL "\
            + "      )"\
            + " SELECT DISTINCT ON (users.username) "\
            + "      users.username , "\
            + "      earth_distance(ll_to_earth(%s, %s), ll_to_earth(users.lat, users.lng)) as distance, "\
            + "      users.lat AS lat, "\
            + "      users.lng AS lng, "\
            + "      array_intersect(array_agg(interest.interest_name), uinterest.uinterest) as interests, "\
            + "      CASE WHEN EXISTS (SELECT 1 from rosterusers WHERE username = %s "\
            + "         AND users.username = split_part(rosterusers.jid, '@', 1) AND subscription = 'B') "\
            + "      THEN 'friends' "\
            + "      ELSE 'anonymous' "\
            + "      END AS friendship_status "\
            + "      FROM uinterest, users "\
            + "      LEFT OUTER JOIN users_interest on (users.username = users_interest.username) "\
            + "      LEFT OUTER JOIN interest on (users_interest.interest_id = interest.interest_id)"\
            + " WHERE earth_box(ll_to_earth(%s, %s),  %s) @> ll_to_earth(users.lat, users.lng)  "\
            + "      AND CASE WHEN isnumeric(last_seen) THEN last_seen::float ELSE 0 END >=  %s "\
            + "      AND users.username != %s "\
            + "      AND users.username NOT IN  "\
            + "      ("\
            + "      ( SELECT bjids FROM banned_users)"\
            + "      )"\
            + "      AND users.show_location = True"\
            + " GROUP BY  friendship_status, users.username, uinterest.uinterest  "\
            + " ORDER BY users.username, friendship_status DESC;"
        variables = (self.username,
                self.username,
                self.lat,
                self.lng,
                self.username,
                self.lat,
                self.lng,
                self.radius,
                int(time.time() - self.was_online_limit),
                self.username
        )
        records = QueryHandler.get_results(query, variables)
        return records

    def get(self):
        response = {}
        self.request.arguments = merge_body_arguments(self)
        try:
            check_udid_and_apk_version(self)
            self.username = str(self.get_argument('username'))
            self.password = str(self.get_argument('password'))
            user = User(username = self.username, password = self.password)
            user.authenticate()
            self.was_online_limit = int(config.get('nearby_users', 'was_online_limit'))
            self.radius = self.get_argument('radius')
            self.lat = self.get_argument('lat')
            self.lng = self.get_argument('lng')
            nearby_users = self.get_nearby_users()
            response['users'] = nearby_users
            response['info'] = settings.SUCCESS_RESPONSE
            response['status'] = settings.STATUS_200
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

class RegisterMatchHandler(tornado.web.RequestHandler):
    """
    This class handles the registration of a match for a jid
    """
    def set_user_match(self):
        query = " INSERT INTO users_matches (username, match_id) VALUES (%s, %s);"
        variables = (self.username, self.match_id)
        QueryHandler.execute(query, variables)

    def post(self):
        response = {}
        try:
            check_udid_and_apk_version(self)
            self.request.arguments = merge_body_arguments(self)
            self.username = str(self.get_argument('username'))
            self.password = str(self.get_argument('password'))
            user = User(username = self.username, password = self.password)
            user.authenticate()
            self.match_id = str(self.get_argument('match_id'))
            self.set_user_match()
            response["info"], response["status"] = settings.SUCCESS_RESPONSE, settings.STATUS_200
        except BadAuthentication, status:
            response["info"] = status.log_message
            response["status"] = settings.STATUS_400
        except MissingArgumentError, status:
            response["info"] = status.log_message
            response["status"] = settings.STATUS_400
        except IntegrityError, status:
            response["info"] = " Specified Match does not exists or has been already subscribed to"
            response["status"] = settings.STATUS_400
        except Exception, e:
            response['info'] = "Error: %s" % e
            response["status"] = settings.STATUS_500
        finally:
            self.write(response)

class UnRegisterMatchHandler(tornado.web.RequestHandler):
    """
    This class handles the registration of a match for a jid
    """
    def remove_user_match(self):
        query = "  DELETE FROM users_matches WHERE users_matches.username = %s AND users_matches.match_id = %s;"
        variables = (self.username, self.match_id)
        QueryHandler.execute(query, variables)

    def post(self):
        response = {}
        try:
            check_udid_and_apk_version(self)
            self.request.arguments = merge_body_arguments(self)
            self.username = str(self.get_argument('username'))
            self.password = str(self.get_argument('password'))
            user = User(username = self.username, password = self.password)
            user.authenticate()
            self.match_id = str(self.get_argument('match_id'))
            self.remove_user_match()
            response["info"], response["status"] = settings.SUCCESS_RESPONSE, settings.STATUS_200
        except BadAuthentication, status:
            response["info"] = status.log_message
            response["status"] = settings.STATUS_400
        except MissingArgumentError, status:
            response["info"] = status.log_message
            response["status"] = settings.STATUS_400
        except Exception, e:
            response['info'] = "Error: %s" % e
            response["status"] = settings.STATUS_500
        finally:
            self.write(response)
            
class AndroidSetUserDeviceToken(tornado.web.RequestHandler):
    """
    This class handles the registration of a match for a jid
    """
    def set_android_device_token(self):
        query = "  UPDATE users SET android_token = %s, device_id = %s WHERE username = %s;"
        variables = ( self.token, self.udid, self.username)
        QueryHandler.execute(query, variables)

    def post(self):
        response = {}
        try:
            check_udid_and_apk_version(self)
            self.request.arguments = merge_body_arguments(self)
            self.username = str(self.get_argument('username'))
            self.password = str(self.get_argument('password'))
            user = User(username = self.username, password = self.password)
            user.authenticate()
            self.udid = str(self.get_argument('udid'))
            self.token = str(self.get_argument('token'))
            self.set_android_device_token()
            response["info"], response["status"] = settings.SUCCESS_RESPONSE, settings.STATUS_200
        except BadAuthentication, status:
            response["info"] = status.log_message
            response["status"] = settings.STATUS_400
        except MissingArgumentError, status:
            response["info"] = status.log_message
            response["status"] = settings.STATUS_400
        except Exception, e:
            response['info'] = "Error: %s" % e
            response["status"] = settings.STATUS_500
        finally:
            self.write(response)

class AndroidRemoveUserDeviceId(tornado.web.RequestHandler):
    """
    This class handles the registration of a match for a jid
    """
    def remove_android_device_token(self):
        query = "  UPDATE users SET android_token = null WHERE username = %s;"
        variables = ( self.username,)
        QueryHandler.execute(query, variables)

    def post(self):
        response = {}
        try:
            check_udid_and_apk_version(self)
            self.request.arguments = merge_body_arguments(self)
            self.username = str(self.get_argument('username'))
            self.password = str(self.get_argument('password'))
            user = User(username = self.username, password = self.password)
            user.authenticate()
            self.remove_android_device_token()
            response["info"], response["status"] = settings.SUCCESS_RESPONSE, settings.STATUS_200
        except BadAuthentication, status:
            response["info"] = status.log_messages
            response["status"] = settings.STATUS_400
        except MissingArgumentError, status:
            response["info"] = status.log_message
            response["status"] = settings.STATUS_400
        except Exception, e:
            response['info'] = "Error: %s" % e
            response["status"] = settings.STATUS_500
        finally:
            self.write(response)

class LocationPrivacyHandler(tornado.web.RequestHandler):
    def set_location_privacy(self):
        query = " UPDATE users SET show_location = %s WHERE username = %s;"
        variables = (self.show_location_status, self.username,)
        QueryHandler.execute(query, variables)

    def post(self):
        response = {}
        try:
            check_udid_and_apk_version(self)
            self.request.arguments = merge_body_arguments(self)
            self.username = str(self.get_argument('username'))
            self.password = str(self.get_argument('password'))
            self.show_location_status = str(self.get_argument('show_location_status'))
            if not self.show_location_status in ["true", "false"]:
                raise BadInfoSuppliedError("location_status")
            user = User(username = self.username, password = self.password)
            user.authenticate()
            self.set_location_privacy()
            response["info"], response["status"] = settings.SUCCESS_RESPONSE, settings.STATUS_200
        except BadAuthentication, status:
            response["info"] = status.log_messages
            response["status"] = settings.STATUS_400
        except MissingArgumentError, status:
            response["info"] = status.log_message
            response["status"] = settings.STATUS_400
        except BadInfoSuppliedError, status:
            response["info"] = status.log_message
            response["status"] = settings.STATUS_400
        except Exception, e:
            response['info'] = "Error: %s" % e
            response["status"] = settings.STATUS_500
        finally:
            self.write(response)

class PushNotificationHandler(tornado.web.RequestHandler):
    def post(self):
        response = {}
        try:
            self.request.arguments = merge_body_arguments(self)
            payload = self.request.body
            match_id = str(self.get_argument('m'))
            NotificationHandler(match_id, payload).notify()
            response["info"], response["status"] = settings.SUCCESS_RESPONSE, settings.STATUS_200
        except MissingArgumentError, status:
            response["info"] = status.log_message
            response["status"] = settings.STATUS_400
        except Exception, e:
            response['info'] = "Error: %s" % e
            response["status"] = settings.STATUS_500
        finally:
            self.write(response)

class Application(tornado.web.Application):
    def __init__(self):
        handlers = [
            (r"/register", RegistrationHandler),
            (r"/create", CreationHandler),
            (r"/set_location", SetLocationHandler),
            (r"/get_nearby_users", GetNearbyUsers),
            (r"/fb_friends", FacebookHandler),
            (r"/football_notifications", FootballEvents),
            (r"/tennis_notifications", TennisEvents),
            (r"/media", MediaHandler),
            (r"/media_present", MediaPresentHandler),
            (r"/media_multipart", IOSMediaHandler),
            (r"/cricket_notifications", CricketEvents),
            (r"/set_user_interests", UserInterestHandler),
            (r"/set_ios_udid", IOSSetUserDeviceId),
            (r"/get_contact_jids", ContactJidsHandler),
            (r"/send_app_invite", SendAppInvitation),
            (r"/user_register_match", RegisterMatchHandler),
            (r"/user_unregister_match", UnRegisterMatchHandler),
            (r"/set_android_token", AndroidSetUserDeviceToken),
            (r"/remove_android_token", AndroidRemoveUserDeviceId),
            (r"/set_location_privacy", LocationPrivacyHandler),
            (r"/notify_event", PushNotificationHandler),

            (r"/admin", admin_api.AdminPage),
            (r"/get_users", admin_api.AdminSelectUsers),
            (r"/create_user", admin_api.AdminCreateUser),
            (r"/update_user", admin_api.AdminUpdateUser),
            (r"/delete_user", admin_api.AdminDeleteUser),
            (r"/block_user", admin_api.AdminBlockUser),

        ]
        settings = dict(
            static_path=os.path.join(os.path.dirname(__file__), "static"),
            autoescape=None
        )
        tornado.web.Application.__init__(self, handlers, **settings)


def add_templates_for_tornado_watch(watched_files):
    for file_name in watched_files:
        tornado.autoreload.watch(settings.ADMIN_TEMPLATES_PATH + file_name)


if __name__ == "__main__":
    app = Application()
    options.log_file_prefix  = "tornado_log"
    enable_pretty_logging(options=options)
    app.listen(int(config.get('tornado', 'listening_port')))
    tornado.autoreload.start()
    add_templates_for_tornado_watch(settings.ADMIN_TEMPLATES)
    tornado.ioloop.IOLoop.current().start()
