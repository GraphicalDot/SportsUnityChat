from global_func import QueryHandler, S3Handler
from notification_adapter import NotificationAdapter
from tornado.log import enable_pretty_logging
from tornado.options import options
import time
import tornado.ioloop
import tornado.web
import random
import tornado
import requests
import os
import facebook
import json
import register
import ConfigParser
from IPython import embed
import base64
from requests_toolbelt import MultipartDecoder

config = ConfigParser.ConfigParser()
config.read('config.py')


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
                :success => {'status': 200, 'info': 'Success'}
                :failure => {'status': 500, 'info': 'Error [Error message]'}

    """

    def get(self):
        response = {}
        try:
            user = str(self.get_arguments("user", True)[0])
            longtitude = float(self.get_arguments("lng", True)[0])
            latitude = float(self.get_arguments("lat", True)[0])
            username = str.split(user, "@")[0]
            query = " UPDATE users SET lat = %s, lng = %s " \
                    " WHERE username = %s; "
            QueryHandler.execute(query, (latitude, longtitude, username))
        except Exception, e:
            response["info"] = " Error %s " % e
            response["status"] = 500
        else:
            response['message'] = "Success"
            response["status"] = 200
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
        _exists -- checks whether the user exists or not   
        _create_new -- creates a new user after verifying the authenticity of the user
        handle_registration -- starts the registration process for the user
        _delete_registered -- deletes the registered user so that new otp can be sent to the user
        _register -- registers the user
        _send_message -- send the otp token to the users phone number
    """
    def __init__(self, username, password = None):
        self.username = username
        self.password = str(password)

    def authenticate(self):
        """
        In case of already created user, this functions authicates the user. 
        return true if authenticated
        return false if not authenticated
        """
        query = " SELECT * FROM users WHERE username = %s AND password = %s; "
        variables = (self.username, self.password,)

        record = QueryHandler.get_results(query, variables)

        if record:
            return True
        else:
            return False

    def handle_creation(self, auth_code):
        """
        Handles the creation aspect of the User class
        Parameters:- 
            auth_code - The otp token sent to the users cellphone
        Response :-
            If token correct:
                "Success", 200, [password]
            If user already created
                " User already created", 200, [password]
            If wrong or expired auth token
                " Wrong or Expired Token ", 400, None
        """
        if self._is_token_correct(auth_code):
            is_created, password = self._exists()
            if not is_created:
                response, status = self._create_new()
                password = self.password
            else:
                response, status = " User already created ", 200
                self.password = password
        else:
            response, status, password = " Wrong or Expired Token ", 400, None
        self._delete_registered()
        return response, status, password

    def _is_token_correct(self, auth_code):
        """
        Authenticates the otp token sent to the user
        Parameters :-
            auth_code -- The otp token sent to the user
        Response :-
            True if correct
            False if wrong token
        """
        query = " SELECT * FROM registered_users WHERE username = %s AND authorization_code = %s ;"
        variables = (self.username, auth_code,)

        record = QueryHandler.get_results(query, variables)
        if record and (record[0]['expiration_time'] > int(time.time())):
            is_token_correct = True
        else:
            is_token_correct = False
        return is_token_correct

    def _exists(self):
        """
        Checks for the existence of the user on the basis of username and the password
        passed to the user class during initialization, also returns the password if 
        user already created.
        Response :-
            True , [password] if user exists
            False, None if user does not exists
        """
        query = " SELECT * FROM users WHERE username = %s;"
        variables = (str.split(self.username, '@')[0],)
        user_info = QueryHandler.get_results(query, variables)
        if len(user_info) == 0:
            registered = False
            password = None
        else:
            password = user_info[0]['password']
            registered = True
        return registered, password

    def _create_new(self):
        """
        Creates a new user in the xmpp directory using the sleek xmpp plugin.
        """
        try:
            registration = register.Register(self.username, self.password)
            registration.register_plugin('xep_0030')
            registration.register_plugin('xep_0004')
            registration.register_plugin('xep_0066')
            registration.register_plugin('xep_0077')
            registration['xep_0077'].force_registration = True
            if registration.connect(('localhost', 5222)):
                registration.process(block=True)
                response, status = "Success", 200
            else:
                response, status = "Failed registration", 500
        except Exception, e:
            response, status = " %s " % e, 500
        finally:
            print " Response in registering users %s " % response
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
        print "deleting registered user"
        query = " DELETE FROM registered_users WHERE username = %s ;"
        variables = (self.username,)
        QueryHandler.execute(query, variables)

    def _register(self):
        """
        Registers the users in the database
        """
        random_integer = random.randint(1000,9999)
        expiration_time = int(time.time()) + int(config.get('registration', 'expiry_period_sec'))

        query = " INSERT INTO registered_users (username, authorization_code, expiration_time) VALUES ( %s, %s, %s); "
        variables = (self.username, random_integer, expiration_time)
        try:
            QueryHandler.execute(query, variables)
            return self._send_message(random_integer)
        except Exception, e:
            return " Error while sending message : % s" % e, 500

    def _send_message(self, random_integer):
        """
        Sends the otp token to the user
        """
        number = str.split(self.username,'@')[0]
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
            return "Success", 200
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
        response = {}
        try:
            fb_id = str(self.get_arguments("fb_id")[0])
            token = str(self.get_arguments("token")[0])
            user_id = str(self.get_arguments('id')[0])

            username = str.split(user_id, "@")[0]

            args = {'fields': 'id,name,email,friends', }
            graph = facebook.GraphAPI(token)
            fb_json = graph.get_object('me', **args)

            fb_name = fb_json['name']

            self._set_fb_details(fb_id, username, fb_name)

            friends_details = self._get_friends_id(fb_json['friends']['data'])
        # TO-DO explicit error messages
        except Exception, e:
            response['info'] = " Error : % s " % e
            response['status'] = 500
        else:
            response['list'] = friends_details
            response['status'] = 200
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
        {'info': 'Success', 'status': 200} if successfully registered 
        {'info': 'Error [Error]', 'status': 500} if not successfully registered 
    """
    def get(self):
        response = {}
        try:
            number = str(self.get_arguments("phone_number")[0])
            username = str.strip(number) + config.get('xmpp', 'domain')
            user = User(username)
            response['info'], response['status'] = user.handle_registration()
        except Exception, e:
            response['info'] = " Error: %s " % e
            response['status'] = 500
        finally:
            self.write(response)


class CreationHandler(tornado.web.RequestHandler):
    def get(self):
        response = {}
        try:
            phone_number = str(self.get_arguments("phone_number")[0])
            username = str.strip(phone_number) + config.get('xmpp', 'domain')
            auth_code = str(self.get_arguments("auth_code")[0])
            password = int(random.random() * 1000000)
            user = User(username, password)
            response['info'], response['status'], response['password'] = user.handle_creation(auth_code)
        except Exception, e:
            response['info'] = " Error %s " % e
            response['status'] = 500
        finally:
            self.write(response)


class ArchiveAcessHandler(tornado.web.RequestHandler):
    def get(self):
        from_timestamp = self.get_arguments("from")
        to_timestamp = self.get_arguments("to")
        skip = self.get_arguments("skip", True) or [0]
        limit = self.get_arguments("limit", True) or [100]
        response = {}
        response['version'] = 0.1
        try:
            response['info'] = QueryHandler.get_results(" SELECT txt, username FROM archive WHERE timestamp > %s " \
                                                        " AND timestamp < %s OFFSET %s LIMIT %s; " \
                                                        , (from_timestamp[0], to_timestamp[0], skip[0], limit[0],))
            response['status'] = 200
        except Exception, e:
            print(e)
            response['info'] = " Error: %s" % e
            response['status'] = 500
        self.write(response)


class GroupsHandler(tornado.web.RequestHandler):
    def get(self):
        skip = self.get_arguments("skip", True) or [0]
        limit = self.get_arguments("limit", True) or [100]
        response = {}
        response['version'] = 0.1
        try:
            response['info'] = QueryHandler.get_results(" SELECT name FROM muc_room OFFSET %s LIMIT %s; " \
                                                        , (skip[0], limit[0],))
            response['status'] = 200
        except Exception, e:
            response['info'] = " Error: %s" % e
            response['status'] = 500
        self.write(response)


class GroupsMessagesHandler(tornado.web.RequestHandler):
    def get(self):
        from_timestamp = self.get_arguments("from")
        to_timestamp = self.get_arguments("to")
        skip = self.get_arguments("skip", True) or [0]
        limit = self.get_arguments("limit", True) or [100]
        response = {}
        response['version'] = 0.1
        try:
            response['info'] = QueryHandler.get_results(" SELECT txt, username FROM archive WHERE timestamp > %s " \
                                                        " AND timestamp < %s AND bare_peer LIKE '%%@conference.mm.io' " \
                                                        " OFFSET %s LIMIT %s; ", \
                                                        (from_timestamp[0], to_timestamp[0], skip[0], limit[0],))
            response['status'] = 200
        except Exception, e:
            response['info'] = " Error: %s" % e
            response['status'] = 500
        self.write(response)


class GroupMessagesHandler(tornado.web.RequestHandler):
    def get(self):
        from_timestamp = self.get_arguments("from")
        to_timestamp = self.get_arguments("to")
        skip = self.get_arguments("skip", True) or [0]
        group = self.get_arguments("group", True) or ["test@conference.mm.io"]
        limit = self.get_arguments("limit", True) or [100]
        response = {}
        response['version'] = 0.1
        try:
            response['info'] = QueryHandler.get_results(" SELECT txt, username FROM archive WHERE timestamp > %s " \
                                                        " AND timestamp < %s AND bare_peer LIKE %s OFFSET %s LIMIT %s;" \
                                                        , (
                    from_timestamp[0], to_timestamp[0], group[0], skip[0], limit[0],))
            response['status'] = 200
        except Exception, e:
            response['info'] = " Error: %s" % e
            response['status'] = 500
        self.write(response)


class UserGroupMessagesHandler(tornado.web.RequestHandler):
    def get(self):
        from_timestamp = self.get_arguments("from")
        to_timestamp = self.get_arguments("to")
        skip = self.get_arguments("skip", True) or [0]
        user = self.get_arguments("user", True) or ['satish@mm.io']
        limit = self.get_arguments("limit", True) or [100]
        response = {}
        response['version'] = 0.1
        try:
            response['info'] = QueryHandler.get_results(" SELECT txt, username FROM archive WHERE timestamp > %s " \
                                                        " AND timestamp < %s AND username LIKE %s AND bare_peer " \
                                                        " LIKE '%%@conference.mm.io' OFFSET %s LIMIT %s; " \
                                                        , (from_timestamp[0], to_timestamp[0], user[0], skip[0], limit[0],))
            response['status'] = 200
        except Exception, e:
            response['info'] = " Error: %s" % e
            response['status'] = 500
        self.write(response)


class PubSubEventHandler(tornado.web.RequestHandler):
    def get(self):
        response = {}
        try:
            name = self.get_arguments("name")[0]

        except Exception, e:
            response['message'] = " Internal Server Error "
            response['status'] = 500
        except NameError:
            response['message'] = " Proper parameters not supplied "
            response['status'] = 500
        else:
            response['message'] = "Success"
            response['status'] = 200
        finally:
            self.write(response)


class ProfilePicHandler(tornado.web.RequestHandler):
    def post(self):
        response = {}
        try:
            profile_pic_bucket = config.get('amazon', 'profile_pics_bucket')
            info = self.request.files['file'][0]
            file_name = info['filename']
            image_file = info['body']
            username = self.get_arguments('username')[0]
            password = self.get_arguments('password')[0]
            user = User(username, password)
            if user.authenticate():
                s3 = S3Handler(profile_pic_bucket)
                s3.upload(username, image_file)
                response['status'] = 200
                response['info'] = 'Success'
            else:
                response['status'] = 400
                response['info'] = 'Invalid Credentials'
        except Exception, e:
            response['status'] = 500
            response['info'] = 'error is: %s' % e
        finally:
            self.write(response)


class MediaPresentHandler(tornado.web.RequestHandler):
    def get(self):
        response = {}
        file_name = "media/" + self.get_arguments("name")[0]
        try:
            if os.path.isfile(file_name):
                response['info'] = 'Present'
                response['status'] = 200
            else:
                response['info'] = 'Not Found'
                response['status'] = 400
        except Exception, e:
            response['status'] = 500
            response['info'] = 'error is: %s' % e
        finally:
            self.write(response)

@tornado.web.stream_request_body
class MediaHandler(tornado.web.RequestHandler):
    
    def prepare(self):
        self.file_content = ''
        
    def post(self):
        try:
            file_name = self.request.headers['Checksum']
            response = {}
            file_name = "media/" + file_name
            if not os.path.isfile(file_name):
                media_file = open(file_name, 'w')
                media_file.write(self.file_content)
                media_file.flush()
            response['status'] = 200
            response['info'] = 'Success'
        except Exception, e:
            response['status'] = 500
            response['info'] = 'error is: %s' % e
        finally:
            self.write(response)

    def get(self):
        response = {}
        try:
            file_name = "media/" + self.get_arguments("name")[0]
            if os.path.isfile(file_name):
                with open(file_name, 'r') as file_content:
                    while 1:
                        data = file_content.read(16384) # or some other nice-sized chunk
                        if not data: break
                        self.write(data)
                    file_content.close()
                    self.finish()
            else:
                response['info'] = 'Not Found'
                response['status'] = 400
                self.write(response)
        except Exception, e:
            response['status'] = 500
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
            response['status'] = 400
            return response

        # 'Checksum' not present in the header
        if response['status'] == 0 and not headers.get('Checksum'):
            response['info'] = " Bad Request: 'Checksum' field not present in the Header!"
            response['status'] = 400
            return response

        # body not present
        if response['status'] == 0 and not body:
            response['info'] = " Bad request: Request body not present!"
            response['status'] = 400
        return response

    def post(self):
        response = {}
        try:
            headers = self.request.headers
            body = self.request.body

            # data validation
            response = self.data_validation(headers, body)

            if response['status'] != 400:
                decoder = MultipartDecoder(body, content_type=headers.get('Content-Type'))
                file_content = decoder.parts[0].content
                file_name = "media/" + headers.get('Checksum')
                if not os.path.isfile(file_name):
                    media_file = open(file_name, 'w')
                    media_file.write(file_content)
                    media_file.flush()
                response['status'] = 200
                response['info'] = 'Success'

        except Exception as e:
            response['status'] = 500
            response['info'] = " Error is: %s" % e
        finally:
            self.write(response)

class GetNearbyUsers(tornado.web.RequestHandler):
    def get(self):
        response = {}
        try:
            self.radius = self.get_arguments('radius')[0]
            self.lat = self.get_arguments('lat')[0]
            self.lng = self.get_arguments('lng')[0]
            users = self.get_nearby_users()
            response['status'] = 200
            response['info'] = 'Success'
            response['users'] = users
        except Exception, e:
            response['status'] = 500
            response['info'] = 'Error: %s' % e
        finally:
            self.write(response)    


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
                :success => {'status': 200, 'info': 'Success'}
                :failure => {'status': 500, 'info': 'Error [Error message]'}     
    """
    def get(self):
        response = {}
        try:
            username = self.get_arguments('username')[0]
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
            response['status'] = 200
            response['info'] = "Success"
        except Exception, e:
            response['status'] = 500
            response['info'] = "Error: %s " % e
        finally:
            self.write(response)
                     
class FootballEvents(tornado.web.RequestHandler):
    def post(self):
        response = {}
        event = tornado.escape.json_decode(self.request.body)
        if event:
            NotificationAdapter(event, "Football").notify()


class TennisEvents(tornado.web.RequestHandler):
    def post(self):
        response = {}
        event = tornado.escape.json_decode(self.request.body)
        if event:
            NotificationAdapter(event, "Tennis").notify()

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


def make_app():
    return tornado.web.Application([
                                       (r"/messages", ArchiveAcessHandler),
                                       (r"/groups", GroupsHandler),
                                       (r"/groups_messages", GroupsMessagesHandler),
                                       (r"/group_messages", GroupMessagesHandler),
                                       (r"/user_group", UserGroupMessagesHandler),
                                       (r"/register", RegistrationHandler),
                                       (r"/create", CreationHandler),
                                       (r"/set_location", SetLocationHandler),
                                       (r"/retrieve_nearby_users", GetNearbyUsers),
                                       (r"/fb_friends", FacebookHandler),
                                       (r"/profile_pic", ProfilePicHandler),
                                       (r"/football_notifications", FootballEvents),
                                       (r"/tennis_notifications", TennisEvents),
                                       (r"/media", MediaHandler),
                                       (r"/media_present", MediaPresentHandler),
                                       (r"/media_multipart", IOSMediaHandler),
                                       (r"/cricket_notifications", CricketEvents),
                                       (r"/set_user_interests", UserInterestHandler),
                                       ],
                                   autoreload = True,
                                   )

if __name__ == "__main__":
    app = make_app()
    options.log_file_prefix  = "tornado_log"
    enable_pretty_logging(options=options)
    app.listen(3000)
    tornado.ioloop.IOLoop.current().start()
