import psycopg2.errorcodes
from models.user import User
from common.funcs import QueryHandler, S3, merge_dicts, send_message, merge_body_arguments, get_short_url, send_threaded_message
from tornado.log import enable_pretty_logging
from tornado.options import options
import magic
import settings
from tornado.web import MissingArgumentError
from psycopg2.extras import Json
from psycopg2 import IntegrityError
import logging
import base64
import ConfigParser
import facebook
import json
import tornado
import os
import random
import requests
import time
import uuid
from models.dp import Dp
from models.user_media import UserMedia
from requests_toolbelt import MultipartDecoder
from models.article_discussion_group import ArticleDiscussionGroup
import threading
from common.custom_error import BadAuthentication, BadInfoSuppliedError, InternalServerError
# import admin_api
from common.notification_handler import NotificationHandler
from models.discussion import Discussion
config = ConfigParser.ConfigParser()
config.read('config.py')

def check_udid_and_apk_version(request_handler_object):
    request_handler_object.get_argument('apk_version')
    request_handler_object.get_argument('udid')


class BaseRequestHandler(tornado.web.RequestHandler):
    def prepare(self):
        logging.debug("[info] Class {} via {} with body {}".format(self.__class__.__name__, self.request.uri, self.request.body))


class UserApiRequestHandler(BaseRequestHandler):

    def prepare(self):
        super(UserApiRequestHandler, self).prepare()
        self.request.arguments = merge_body_arguments(self)
        check_udid_and_apk_version(self)
        self.username = self.get_argument('username')
        self.password = self.get_argument('password')
        user = User(username = self.username, password = self.password)
        user.authenticate()
        logging.debug("[debug] User {} authenticated".format(self.username))

    def extract_psycopg2_integrity_error(self, error):
        return error.message.split("Key")[1].replace("(", "").replace(")", "").split(".")[0].replace("=", " ")
        
    def write_error(self, status_code, **kwargs):
        response = {}
        error_object = kwargs['exc_info'][1]
        error_type = kwargs['exc_info'][0]
        try:
            if error_type == IntegrityError:
                response["info"] = self.extract_psycopg2_integrity_error(error_object)
            else:
                try:
                    response["info"] = error_object.log_message
                except:
                    response["info"] = error_object.message
        except Exception, e:
            response["info"] = settings.INTERNAL_SERVER_ERROR
            
        if error_type == BadInfoSuppliedError:
            response["status"] = settings.STATUS_400
        elif error_type == MissingArgumentError:
            response["status"] = settings.STATUS_400
        elif error_type == ValueError:
            response["info"] = "Improper JSON format "
            response["status"] = settings.STATUS_400
        elif error_type == BadAuthentication:
            response["status"] = settings.STATUS_404
        elif error_type == KeyError:
            response["status"] = settings.STATUS_400
        elif error_type == IntegrityError:
            response["status"] = settings.STATUS_400
        else:
            response["status"] = settings.STATUS_500
        self.write(response)


class SetLocationHandler(UserApiRequestHandler):
    # TO-DO Write tests for this class
    """
    This class handles the storage of locations of a user in the server
    For web interfacing it implements two methods i.e get and post, which 
    corresponds to the tornado framework requirements. 
    Methods : 
        get :
            :params 
                username username
                password password  
                lng  longtitude
                lat  latitude

            :response 
                :success => {'status':settings.STATUS_200, 'info': 'Success'}
                :failure => {'status': 500, 'info': 'Error [Error message]'}

    """
    def set_location(self):
        query = " UPDATE users SET lat = %s, lng = %s " \
                " WHERE username = %s; "
        QueryHandler.execute(query, (self.latitude, self.longtitude, self.username))


    def get(self):
        response = {}
        self.username = self.get_argument("username")
        self.longtitude = self.get_argument("lng")
        self.latitude = self.get_argument("lat")
        self.set_location()
        response['message'] = settings.SUCCESS_RESPONSE
        response["status"] = settings.STATUS_200
        self.write(response)




# class FacebookHandler(tornado.web.RequestHandler):
#     """
#     Stores the facebook id of the user and finds his friends from the stored ids
#     Methods:
#         get - Implements the http get method for tornado frameowrk and also sets the user
#             facebook details in the database
#             Parameters:
#                 fb_id -- fb_id of the user
#                 token -- fb token of the user
#                 user_id -- xmpp user_id 
#             Response: 
#                 {'info' : 'Error [Error]', 'status': 500}
#                 {'info' : [{'id': [friend_id], 'fb_name': [friend name]} .. ], 'status': 500}
#         _get_friends_id - 
#             Gets friends facebook id. 
#         _set_fb_details - 
#             sets the users fb details in the database
#     """
#     def get(self):
#         try:
#             check_udid_and_apk_version(self)
#             response = {}
#             fb_id = str(self.get_argument("fb_id"))
#             token = str(self.get_argument("token"))
#             user_id = str(self.get_argument('id'))

#             username = str.split(user_id, "@")[0]

#             args = {'fields': 'id,name,email,friends', }
#             graph = facebook.GraphAPI(token)
#             fb_json = graph.get_object('me', **args)

#             fb_name = fb_json['name']

#             self._set_fb_details(fb_id, username, fb_name)

#             friends_details = self._get_friends_id(fb_json['friends']['data'])
#         # TO-DO explicit error messages
#         except MissingArgumentError, status:
#             response["info"] = status.log_message 
#             response["status"] = settings.STATUS_400
#         except Exception, e:
#             response['info'] = " Error : % s " % e
#             response['status'] = settings.STATUS_500
#         else:
#             if not response:
#                 response['list'] = friends_details
#                 response['status'] = settings.STATUS_200
#         finally:
#             self.write(response)

#     def _get_friends_id(self, friends_details):
#         friends_id_name = []
#         for friend_detail in friends_details:
#             friend_id_name = {}
#             query = " SELECT * FROM users WHERE fb_id = %s;"
#             variables = (friend_detail['id'],)
#             results = QueryHandler.get_results(query, variables)
#             if len(results) > 0:
#                 friend_id_name['id'] = results['username'] + '@mm.io'
#                 friend_id_name['fb_name'] = results['fb_name'] + '@mm.io'
#                 friends_id_name.append(friend_id_name)
#         return friends_id_name

#     def _set_fb_details(self, fb_id, username, fb_name):
#         query = " UPDATE users SET fb_id = %s, fb_name = %s WHERE username = %s ;"
#         variables = (fb_id, fb_name, username)
#         QueryHandler.execute(query, variables)


class RegistrationHandler(BaseRequestHandler):
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


class CreationHandler(BaseRequestHandler):
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
            response['info'],\
            response['status'],\
            response['password'],\
            response['username'],\
            response['name'],\
            response['interests'],\
            response['user_status'],\
            response['photo'],\
            response["friends"] = user.handle_creation(auth_code)
        except MissingArgumentError, status:
            response["info"] = status.log_message 
            response["status"] = settings.STATUS_400
        except Exception, e:
            response['info'] = " Error %s " % e
            response['status'] = settings.STATUS_500
        finally:
            self.write(response)

class MediaPresentHandler(BaseRequestHandler):
    def get(self):
        check_udid_and_apk_version(self)
        response = {}
        try:
            file_name = self.get_argument("name")
            if UserMedia(name = file_name).exists():
                response['info'] = 'Present'
                response['status'] = settings.STATUS_200
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
class MediaHandler(BaseRequestHandler):
    response = {}

    def prepare(self):
        self.file_content = ''

    def post(self):
        response = {}
        try:
            check_udid_and_apk_version(self)
            file_name = self.request.headers['Checksum']
            if not UserMedia(name = file_name).exists():
                UserMedia(name = file_name, content = self.file_content).upload()
            response['status'] = settings.STATUS_200
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
            file_name = self.get_argument("name")
            file_content = UserMedia(name = file_name).download()
            chunk_length = 16384
            for part in [file_content[0+i: chunk_length + i] for i in xrange(0, len(file_content), chunk_length)]:
                self.write(part)
                self.flush()
        except ClientError:
            response['info'] = 'Not Found'
            response['status'] = settings.STATUS_400
            self.write(response)
        except MissingArgumentError, status:
            response["info"] = status.log_message 
            response["status"] = settings.STATUS_400
            self.write(response)
        except Exception, e:
            response['status'] = settings.STATUS_500
            response['info'] = 'error is: %s' % e
            self.write(response)

    def data_received(self, data):
        self.file_content += data


class IOSMediaHandler(BaseRequestHandler):

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


class UserInterestHandler(UserApiRequestHandler):
    """
    This class creates a link between users and interests. The interests have to
    stored beforehand.

    Methods : 
        post :
            :params 
                username => username 
                interests => list of interests like interests=football&interests=cricket 
                password => password
                apk_version 
                udid
            :response 
                :success => {'status':settings.STATUS_200, 'info': 'Success'}
                :failure => {'status': 500, 'info': 'Error [Error message]'}     
    """
    def get_user_interests(self):
        query = " SELECT interest_id FROM users_interest WHERE username = %s;"
        variables = (self.username,)
        return QueryHandler.get_results(query, variables)

    def insert_user_interest(self, interests):
        query = "INSERT INTO users_interest (interest_id, username) "\
            " (SELECT interest_id, %s FROM interest WHERE "\
            + " OR ".join(map( lambda interest: "interest_id = '" + str(interest) + "'" , interests))\
            + ");"         
        variables = (self.username, )
        QueryHandler.execute(query, variables)

    def delete_user_interest(self, interests):
        query = " DELETE FROM users_interest WHERE "\
        +   "username = %s AND ( " + " OR ".join(["interest_id = %s "]*len(interests)) + " );"
        variables = [self.username] + interests
        QueryHandler.execute(query, variables)

    def delete_all_user_interest(self):
        query = " DELETE FROM users_interest WHERE "\
        +   " username = %s ;"
        variables = (self.username,)
        QueryHandler.execute(query, variables)

    def post(self):
        response = {}
        self.delete_all_user_interest()
        self.username = self.get_argument('username')
        interests = self.request.arguments['interests']
        if interests and type(interests) == list:
            self.insert_user_interest(interests)
        else:
            raise BadInfoSuppliedError("interests")
        response['status'] = settings.STATUS_200
        response['info'] = settings.SUCCESS_RESPONSE
        self.write(response)

    def delete(self):
        response = {}
        self.username = self.get_argument('username')
        interests = self.request.arguments['interests']
        self.delete_user_interest(interests)

        response['status'] =settings.STATUS_200
        response['info'] = settings.SUCCESS_RESPONSE
        self.write(response)        


class IOSSetUserDeviceTokenReturnsUsersMatches(UserApiRequestHandler):

    def set_ios_token_and_return_user_matches(self): 
        token_type = settings.TOKEN_IOS_TYPE
        query = " WITH updated AS (UPDATE users SET device_token=%s, token_type = %s, device_id = %s WHERE username=%s) "\
        +   "SELECT users_matches.match_id FROM users_matches WHERE users_matches.username = %s;"
        variables = (self.token, token_type, self.udid, self.username, self.username)
        return QueryHandler.get_results(query, variables)

    def post(self):
        response = {}
        self.username = str(self.get_argument('username'))
        self.token = str(self.get_argument('token'))
        self.udid = str(self.get_argument('udid'))
        user = User(password = self.password, username = self.username)
        user.authenticate()
        users_matches = self.set_ios_token_and_return_user_matches()
        response['match_ids'] = map(lambda x: x['match_id'], users_matches)
        response['info'] = settings.SUCCESS_RESPONSE
        response['status'] =settings.STATUS_200
        self.write(response)


class SendAppInvitation(tornado.web.RequestHandler):
    """
    Sends invitation invite from app user to any of its contacts.
    """

    def post(self):
        response = {}
        self.phone_number = str(self.get_argument('phone_number')).strip()
        self.message = settings.APP_INVITATION_MESSAGE
        threading.Thread(group = None, target = self.handle_message_sending, name = None, args = ()).start()
        
        response['info'] = settings.SUCCESS_RESPONSE
        response['status'] = settings.STATUS_200
        self.write(response)

    def handle_message_sending(self):
        status_info, status_code, gateway_response = send_message(number=self.phone_number, message=self.message)

        query = " INSERT INTO invited_users (phone_number, gateway_response) VALUES (%s, %s);"
        variables = (self.phone_number, Json(gateway_response), )
        QueryHandler.execute(query, variables)





# class FootballEvents(tornado.web.RequestHandler):
#     def post(self):
#         event = tornado.escape.json_decode(self.request.body)
#         if event:
#             NotificationAdapter(event, "Football").notify()


# class TennisEvents(tornado.web.RequestHandler):
#     def post(self):
#         event = tornado.escape.json_decode(self.request.body)
#         if event:
#             NotificationAdapter(event, "Tennis").notify()


# class CricketEvents(tornado.web.RequestHandler):
#     def post(self):
#         event = tornado.escape.json_decode(self.request.body)
#         if event:
#             NotificationAdapter(event, "Cricket").notify()


class ContactJidsHandler(UserApiRequestHandler):
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
        username = self.get_argument('username')
        contacts = self.get_arguments('contacts')
        if not (type(contacts) == list and len(contacts) > 0): 
            raise BadInfoSuppliedError("Contacts") 
        response['info'] = settings.SUCCESS_RESPONSE
        response['status'] = settings.STATUS_200
        response['jids'] = self.get_contacts_jids(username, contacts)
        self.write(response)


class GetNearbyUsers(UserApiRequestHandler):

    def get_nearby_users(self):
        query = " WITH user_pref AS"\
            + "      ( "\
            + "            SELECT show_location FROM users WHERE username = %s AND show_location != %s "\
            + "      ),"\
            + " uinterest AS "\
            + "      ( "\
            + "            SELECT array_agg(interest.interest_name) AS uinterest FROM interest, users_interest  "\
            + "            WHERE users_interest.username = %s AND users_interest.interest_id = interest.interest_id "\
            + "      ),"\
            + " user_roster AS "\
            + "      ( "\
            + "            SELECT split_part(rosterusers.jid, '@', 1) AS friends from rosterusers WHERE username = %s "\
            + "               AND subscription = 'B' "\
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
            + "      users.username , users.name, "\
            + "      earth_distance(ll_to_earth(%s, %s), ll_to_earth(users.lat, users.lng)) as distance, "\
            + "      users.lat AS lat, "\
            + "      users.is_available AS is_available, "\
            + "      users.last_seen AS last_seen, "\
            + "      users.lng AS lng, "\
            + "      CASE WHEN users.username IN (SELECT friends FROM user_roster) THEN 'friends' ELSE 'anonymous' END AS friendship_status, "\
            + "      array_intersect(array_agg(interest.interest_name), uinterest.uinterest) as interests "\
            + "      FROM user_pref, uinterest, users "\
            + "      LEFT OUTER JOIN users_interest on (users.username = users_interest.username) "\
            + "      LEFT OUTER JOIN interest on (users_interest.interest_id = interest.interest_id)"\
            + " WHERE earth_box(ll_to_earth(%s, %s),  %s) @> ll_to_earth(users.lat, users.lng)  "\
            + "      AND users.username != %s "\
            + "      AND users.username NOT IN  "\
            + "      ("\
            + "          SELECT bjids FROM banned_users"\
            + "      )"\
            + "      AND users.type = %s"\
            + "      AND "\
            + "      ("\
            + "         CASE "\
            + "             WHEN users.show_location = 'n' THEN False "\
            + "             WHEN  user_pref.show_location = 'a' AND users.show_location = 'a' OR (users.username IN (SELECT friends FROM user_roster) AND users.show_location = 'f' ) THEN True "\
            + "             WHEN user_pref.show_location = 'f' AND users.username IN (SELECT friends FROM user_roster) AND (users.show_location = 'a' OR users.show_location = 'f') THEN True "\
            + "             ELSE False "\
            + "         END "\
            + "      ) = True"\
            + " GROUP BY  friendship_status, users.username, uinterest.uinterest  "\
            + " ORDER BY users.username, friendship_status DESC;"
        variables = (self.username,
                settings.SHOW_LOCATION_NONE_STATUS,
                self.username,
                self.username,
                self.username,
                self.lat,
                self.lng,
                self.lat,
                self.lng,
                self.radius,
                self.username,
                settings.USER_TYPE_HUMAN,
        )
        records = QueryHandler.get_results(query, variables)
        return records

    def get(self):
        response = {}
        self.username = str(self.get_argument('username'))
        self.was_online_limit = int(config.get('nearby_users', 'was_online_limit'))
        self.radius = self.get_argument('radius')
        self.lat = self.get_argument('lat')
        self.lng = self.get_argument('lng')
        nearby_users = self.get_nearby_users()
        response['users'] = nearby_users
        response['info'] = settings.SUCCESS_RESPONSE
        response['status'] = settings.STATUS_200
        self.write(response)

class RegisterUserMatchHandler(UserApiRequestHandler):
    """
    This class handles the registration of a match for a jid
    """
    def set_user_match(self):
        query = " INSERT INTO users_matches (username, match_id) VALUES (%s, %s);"
        variables = (self.username, self.match_id)
        try:
            QueryHandler.execute(query, variables)
        except Exception, e:
            if e.pgcode == psycopg2.errorcodes.UNIQUE_VIOLATION:
                pass
            else:
                raise e

    def post(self):
        response = {}
        self.username = str(self.get_argument('username'))
        self.match_id = str(self.get_argument('match_id'))
        self.set_user_match()
        response["info"], response["status"] = settings.SUCCESS_RESPONSE, settings.STATUS_200
        self.write(response)

class UnRegisterUserMatchHandler(UserApiRequestHandler):
    """
    This class handles the registration of a match for a jid
    """
    def remove_user_match(self):
        query = "  DELETE FROM users_matches WHERE users_matches.username = %s AND users_matches.match_id = %s;"
        variables = (self.username, self.match_id)
        QueryHandler.execute(query, variables)

    def post(self):
        response = {}
        self.username = str(self.get_argument('username'))
        self.match_id = str(self.get_argument('match_id'))
        self.remove_user_match()
        response["info"], response["status"] = settings.SUCCESS_RESPONSE, settings.STATUS_200
        self.write(response)
            
class AndroidSetUserDeviceTokenReturnsUsersMatches(UserApiRequestHandler):
    """
    This class handles the registration of a match for a jid
    """
    def set_android_device_token_returning_user_matches(self):
        token_type = settings.TOKEN_ANDROID_TYPE
        query = " WITH updated AS (UPDATE users SET device_token = %s, token_type = %s, device_id = %s WHERE username = %s) "\
            + " SELECT users_matches.match_id FROM users_matches WHERE users_matches.username = %s ;"
        variables = (self.token, token_type, self.udid, self.username, self.username)
        return QueryHandler.get_results(query, variables)

    def post(self):
        response = {}
        self.username = str(self.get_argument('username'))
        self.udid = str(self.get_argument('udid'))
        self.token = str(self.get_argument('token'))
        users_matches = self.set_android_device_token_returning_user_matches()
        response['match_ids'] = map(lambda x: x['match_id'], users_matches)
        response["info"], response["status"] = settings.SUCCESS_RESPONSE, settings.STATUS_200
        self.write(response)

class AndroidRemoveUserDeviceId(UserApiRequestHandler):
    """
    This class handles the registration of a match for a jid
    """
    def remove_android_device_token(self):
        query = "  UPDATE users SET device_token = null WHERE username = %s;"
        variables = ( self.username,)
        QueryHandler.execute(query, variables)

    def post(self):
        response = {}
        self.username = str(self.get_argument('username'))
        self.remove_android_device_token()
        response["info"], response["status"] = settings.SUCCESS_RESPONSE, settings.STATUS_200
        self.write(response)

class LocationPrivacyHandler(UserApiRequestHandler):
    def set_location_privacy(self):
        query = " UPDATE users SET show_location = %s WHERE username = %s;"
        variables = (self.show_location_status, self.username,)
        QueryHandler.execute(query, variables)

    def post(self):
        response = {}
        self.username = str(self.get_argument('username'))
        self.show_location_status = str(self.get_argument('show_location_status'))
        if not self.show_location_status in ["true", "false"]:
            raise BadInfoSuppliedError("location_status")
        self.show_location_status = "a" if self.show_location_status == "true" else "n" 
        self.set_location_privacy()
        response["info"], response["status"] = settings.SUCCESS_RESPONSE, settings.STATUS_200
        self.write(response)

class PushNotificationHandler(BaseRequestHandler):
    def post(self):
        response = {}
        try:
            payload = json.loads(self.request.body)
            match_id = str(payload['m'])
            league_id = str(payload['l'])
            match_league_id = match_id.strip() + "|" + league_id.strip()
            NotificationHandler(match_league_id, payload).notify()
            response["info"], response["status"] = settings.SUCCESS_RESPONSE, settings.STATUS_200
        except MissingArgumentError, status:
            response["info"] = status.log_message
            response["status"] = settings.STATUS_400
        except Exception, e:
            response['info'] = "Error: %s" % e
            response["status"] = settings.STATUS_500
        finally:
            self.write(response)

class RegisterMatchHandler(BaseRequestHandler):
    def insert_match(self, match):
        query = "INSERT INTO matches (id, name) VALUES (%s, %s);"
        variables = (match["id"], match["name"],)
        try:

            QueryHandler.execute(query, variables)
        except IntegrityError:
            pass
            

    def post(self):
        response = {}
        try:
            self.request.arguments = merge_body_arguments(self)
            matches = self.request.arguments["matches"]
            for match in matches:
                self.insert_match(match)
            response["info"], response["status"] = settings.SUCCESS_RESPONSE, settings.STATUS_200
        except Exception, e:
            response['info'] = "Error: %s" % e
            response["status"] = settings.STATUS_500
        finally:
            self.write(response)

class SetUserInfoHandler(UserApiRequestHandler):

    def post(self):
        response = {}
        user_info = {}
        self.username = self.get_argument('username')
        user_info['status'] = self.get_argument('status', None)
        user_info['photo'] = self.get_argument('photo', None)
        user_info['name'] = self.get_argument('name', None)
        user = User(username = self.username)
        user.set_info(user_info)
        response["info"], response["status"] = settings.SUCCESS_RESPONSE, settings.STATUS_200
        self.write(response)

class GetUserInfoHandler(UserApiRequestHandler):

    def post(self):
        response = {}
        user_info = {}
        requested_jid = self.get_argument('r_jid', None)
        requested_info = self.get_arguments('r_info')
        user = User(username = requested_jid)
        response['user_info'] = user.get_info(requested_info)
        response["info"], response["status"] = settings.SUCCESS_RESPONSE, settings.STATUS_200
        self.write(response)


class SetDpHandler(UserApiRequestHandler):
    def post(self):
        response = {}
        jid = self.get_argument('jid')
        content = self.get_argument('content')
        Dp(jid).upload_dp(content)
        response['status'] = 200
        response['info'] = 'Success'
        self.write(response)

class GetDpHandler(UserApiRequestHandler):
    def post(self):
        response = {}
        jid = self.get_argument('jid')
        version = self.get_argument('version')
        content = base64.b64encode(Dp(jid).get_dp_version(version))
        response['status'] = 200
        response['info'] = 'Success'
        response['content'] = content
        self.write(response)        


class GetRefrralCodeHandler(UserApiRequestHandler):
    def get_referral_code(self):
        query = " UPDATE users SET referral_code = COALESCE (referral_code, COALESCE(regexp_replace(LOWER(name::text), '[^[:alpha:]]','' ,'g'), '') || 'u' || substring( md5(random()::text) from 1 for 3) ) WHERE username = %s "\
        +   " RETURNING referral_code ; "
        variables = (self.username,)
        record = QueryHandler.get_results(query, variables)
        return record[0]['referral_code']

    def post(self):
        response = {}
        self.username = self.get_argument("username")
        referral_code = self.get_referral_code()
        play_store_referral_url = settings.PLAY_STORE_URL.format(referral_code)
        referral_url = get_short_url(play_store_referral_url)
        response['status'] = 200
        response['info'] = 'Success'
        response['referral_code'] = referral_code
        response['referral_url'] = referral_url
        self.write(response)       


class RedeemCodeHandler(UserApiRequestHandler):
    def apply_referral_code(self):
        query = " WITH coupon AS (UPDATE coupons SET used_count = used_count + 1 WHERE code = %s AND used_count < coupon_limit RETURNING code) "\
        +   " INSERT INTO referrals (username, referred_by) VALUES (%s, COALESCE((SELECT code FROM coupon), (SELECT username FROM users WHERE referral_code = %s AND username != %s)) ); "
        variables = (self.referral_code, self.username, self.referral_code, self.username, )
        QueryHandler.execute(query, variables)

    def post(self):
        response = {}
        self.username = self.get_argument("username")
        self.referral_code = self.get_argument('referral_code')
        self.apply_referral_code()
        response['status'] = 200
        response['info'] = 'Success'
        self.write(response)       

class FriendsWatchingHandler(UserApiRequestHandler):
    def post(self):
        response = {}
        self.username = self.get_argument("username")
        self.matches = tuple(self.get_arguments('matches'))
        if self.matches:
            friends_watching = {}
            records = self.get_friends_matches()
            for record in records:
                friends_watching.update({record['match_id']: record['friends']})
            response['matches'] = friends_watching
        response['status'] = 200
        response['info'] = 'Success'
        self.write(response)    

    def get_friends_matches(self):
        query = " SELECT match_id, array_agg(username) AS friends FROM users_matches WHERE username IN "\
        +   " (SELECT split_part(rosterusers.jid, '@', 1) from rosterusers WHERE username = %s "\
        +   "   AND subscription = 'B') "\
        +   " AND match_id IN %s GROUP BY match_id; "
        variables = (self.username, self.matches, )
        records = QueryHandler.get_results(query, variables)
        return records


class PollAnswerHandler(UserApiRequestHandler):
    def post(self):
        response = {}
        self.username = self.get_argument("username")
        self.article_id = self.get_argument('article_id')
        self.poll_answer = self.get_argument('poll_answer')
        if not self.poll_answer in settings.ARTICLE_POLL_ANSWER_TYPES:
            raise BadInfoSuppliedError("poll_answer")
        result = self.allocate_group()
        response['status'] = 200
        response['info'] = 'Success'
        self.write(response)

    def allocate_group(self):
        query = " SELECT * FROM assign_discussion(%s, %s, %s);"
        variables = (self.article_id, self.username, self.poll_answer)
        result = QueryHandler.get_results(query, variables)
        self.handle_result(result)

    def handle_result(self, results):
        if results[0]['action_taken'] == "new_discussion":
            discussion_id = results[0]["discussion_id"]
            discussion_info = {"name": results[0]['discussion_id'], "users": map(lambda info: info ["username"], results)}
            Discussion(discussion_id).create_and_add_users(discussion_info)
        elif results[0]['action_taken'] == "existing_discussion":
            discussion_id = results[0]["discussion_id"]
            discussion_info = {"name": results[0]['discussion_id'], "users": map(lambda info: info ["username"], results)}
            Discussion(discussion_id).add_users(discussion_info)
        elif results[0]['action_taken'] == "stored_preference":
            pass
        else:
            raise InternalServerError

class DiscussionHandler(BaseRequestHandler):
    def post(self):
        response = {}
        self.username = self.get_argument("username")
        self.discussion_id = self.get_argument('discussion_id')
        result = self.exit_discussion()
        response['status'] = 200
        response['info'] = 'Success'
        self.write(response)

    def exit_discussion(self): 
        query = " SELECT * FROM exit_discussion(%s, %s);"
        variables = (self.article_id, self.username)
        result = QueryHandler.get_results(query, variables)
        self.handle_result(result)                       

    def handle_result(self, result):
        if results[0]['action_taken'] == 'deleted_discussion':
            Discussion(self.discussion_id).unsubsribe_user_and_delete(results[0]['username'])
        elif results[0]['action_taken'] == "deleted_user_from_discussion":
            pass
        else:
            raise InternalServerError        