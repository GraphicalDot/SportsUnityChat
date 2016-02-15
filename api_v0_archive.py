from global_func import QueryHandler, S3Handler
from notification_adapter import NotificationAdapter
from tornado.log import enable_pretty_logging
from tornado.options import options
import magic
import settings
from tornado.web import MissingArgumentError
import time
import uuid
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
from custom_error import BadAuthentication
config = ConfigParser.ConfigParser()
config.read('config.py')


class RequestsBaseHandler(tornado.web.RequestHandler):
    response = None

    def prepare(self):
        self.response = {}
        if not self.get_argument('apk_version', '') or not self.get_argument('udid', ''):
            self.response['info'] = settings.MISSING_APK_AND_UDID_ERROR
            self.response['status'] = settings.STATUS_400


class SetLocationHandler(RequestsBaseHandler):
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
            self.response["info"] = " Error %s " % e
            self.response["status"] = settings.STATUS_500
        else:
            self.response['message'] = settings.SUCCESS_RESPONSE
            self.response["status"] = settings.STATUS_200
        finally:
            self.write(self.response)


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
            query = " UPDATE users SET password = %s ; " 
            variables = (self.password, )
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


class FacebookHandler(RequestsBaseHandler):
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
            self.response['info'] = " Error : % s " % e
            self.response['status'] = settings.STATUS_500
        else:
            if not self.response:
                self.response['list'] = friends_details
                self.response['status'] = settings.STATUS_200
        finally:
            self.write(self.response)

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


class RegistrationHandler(RequestsBaseHandler):
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
            phone_number = str(self.get_query_argument("phone_number"))
            user = User(phone_number)
            response['info'], response['status'] = user.handle_registration()
        except MissingArgumentError, status:
            response["info"] = status.log_message 
            response["status"] =settings.STATUS_400   
        except Exception, e:
            self.response['info'] = " Error: %s " % e
            self.response['status'] = settings.STATUS_500
        finally:
            self.write(self.response)


class CreationHandler(RequestsBaseHandler):
    def get(self):
        response = {}
        try:
            phone_number = str(self.get_query_argument("phone_number"))
            auth_code = str(self.get_query_argument("auth_code"))
            user = User(phone_number)
            response['info'], response['status'], response['password'], response['username']\
                = user.handle_creation(auth_code)
        except MissingArgumentError, status:
            response["info"] = status.log_message 
            response["status"] =settings.STATUS_400
        except Exception, e:
            self.response['info'] = " Error %s " % e
            self.response['status'] = settings.STATUS_500
        finally:
            self.write(self.response)

class MediaPresentHandler(RequestsBaseHandler):
    def get(self):
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
            self.response['status'] = settings.STATUS_500
            self.response['info'] = 'error is: %s' % e
        finally:
            self.write(self.response)


@tornado.web.stream_request_body
class MediaHandler(tornado.web.RequestHandler):
    response = {}

    def prepare(self):
        self.file_content = ''
        if not self.get_argument('apk_version', '') or not self.get_argument('udid', ''):
            self.response['info'] = "Bad Request: Please provide 'apk_version' and 'udid'"
            self.response['status'] = settings.STATUS_400

    def post(self):
        try:
            file_name = self.request.headers['Checksum']
            response = {}
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
            self.response['status'] = settings.STATUS_500
            self.response['info'] = 'error is: %s' % e
        finally:
            self.write(self.response)

    def get(self):
        try:
            if not self.response:
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
                    self.response['info'] = 'Not Found'
                    self.response['status'] = settings.STATUS_400
                    self.write(self.response)
        except MissingArgumentError, status:
            response["info"] = status.log_message 
            response["status"] =settings.STATUS_400
        except Exception, e:
            self.response['status'] = settings.STATUS_500
            self.response['info'] = 'error is: %s' % e
            self.write(self.response)

    def data_received(self, data):
        self.file_content += data


class IOSMediaHandler(RequestsBaseHandler):

    def data_validation(self, headers, body):
        self.response = {'info': '', 'status': 0}

        # 'Content-Type' not present in the header
        if not headers.get('Content-Type'):
            self.response['info'] = " Bad Request: 'Content-Type' field not present in the Header!"
            self.response['status'] = settings.STATUS_400
            return self.response

        # 'Checksum' not present in the header
        if self.response['status'] == 0 and not headers.get('Checksum'):
            self.response['info'] = " Bad Request: 'Checksum' field not present in the Header!"
            self.response['status'] = settings.STATUS_400
            return self.response

        # body not present
        if self.response['status'] == 0 and not body:
            self.response['info'] = " Bad request: Request body not present!"
            self.response['status'] = settings.STATUS_400
        return self.response

    def post(self):
        try:
            response = {}
            if not self.response:
                headers = self.request.headers
                body = self.request.body

                # data validation
                self.response = self.data_validation(headers, body)

                if self.response['status'] != settings.STATUS_400:
                    decoder = MultipartDecoder(body, content_type=headers.get('Content-Type'))
                    file_content = decoder.parts[0].content
                    file_name = "media/" + headers.get('Checksum')
                    if os.path.isfile(file_name):
                        self.response['status'] = settings.STATUS_422
                        self.response['info'] = "Error: File with same name already exists!"
                    else:
                        media_file = open(file_name, 'w')
                        media_file.write(file_content)
                        media_file.flush()
                        self.response['status'] = settings.STATUS_200
                        self.response['info'] = settings.SUCCESS_RESPONSE
        except MissingArgumentError, status:
            response["info"] = status.log_message 
            response["status"] =settings.STATUS_400
        except Exception as e:
            self.response['status'] = settings.STATUS_500
            self.response['info'] = " Error is: %s" % e
        finally:
            self.write(self.response)


class GetNearbyUsers(RequestsBaseHandler):
    def get(self):
        response = {}
        try:
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
            self.response['status'] = settings.STATUS_500
            self.response['info'] = 'Error: %s' % e
        finally:
            self.write(self.response)


    def get_nearby_users(self):
        query = "SELECT users.username, earth_distance(ll_to_earth(%s, %s),"\
            + " ll_to_earth(users.lat, users.lng)) as distance, users.lat AS lat, users.lng AS lng "\
            + ", string_agg(interest.interest_name, ' ,') as interests "\
            + " FROM users  "\
            + " left outer join users_interest on (users.username = users_interest.username) "\
            + " left outer join interest on (users_interest.interest_id = interest.interest_id)"\
            + " WHERE earth_box(ll_to_earth(%s, %s),  %s) @> ll_to_earth(users.lat, users.lng) AND  "\
            + " is_available = True "\
            + " GROUP BY users.username ORDER BY distance ASC;"
        variables = (self.lat, self.lng, self.lat, self.lng, self.radius)
        records = QueryHandler.get_results(query, variables)
        return records


class UserInterestHandler(RequestsBaseHandler):
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
            self.response['status'] = settings.STATUS_500
            self.response['info'] = "Error: %s " % e
        finally:
            self.write(self.response)


class IOSSetUserDeviceId(RequestsBaseHandler):

    def post(self):
        try:
            response = {}
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
            self.response['info'] = "Error: %s" % e
            self.response['status'] = settings.STATUS_500
        finally:
            self.write(self.response)


class FootballEvents(RequestsBaseHandler):
    def post(self):
        if not self.response:
            event = tornado.escape.json_decode(self.request.body)
            if event:
                NotificationAdapter(event, "Football").notify()


class TennisEvents(RequestsBaseHandler):
    def post(self):
        if not self.response:
            event = tornado.escape.json_decode(self.request.body)
            if event:
                NotificationAdapter(event, "Tennis").notify()


class CricketEvents(RequestsBaseHandler):
    def post(self):
        if not self.response:
            event = tornado.escape.json_decode(self.request.body)
            if event:
                NotificationAdapter(event, "Cricket").notify()


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
                                       ],
                                   autoreload = True,
                                   )

if __name__ == "__main__":
    app = make_app()
    options.log_file_prefix  = "tornado_log"
    enable_pretty_logging(options=options)
    app.listen(3000)
    tornado.ioloop.IOLoop.current().start()
