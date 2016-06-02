from models.user import User
from common.funcs import QueryHandler, S3, merge_dicts, send_message, merge_body_arguments
from tornado.log import enable_pretty_logging
from tornado.options import options
import settings
from tornado.web import MissingArgumentError
import ConfigParser
import json
import psycopg2.extras
from psycopg2 import IntegrityError
import logging
import tornado
from requests_toolbelt import MultipartDecoder
from common.custom_error import BadAuthentication, BadInfoSuppliedError
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


class LocationPrivacyHandler(UserApiRequestHandler):
    def set_location_privacy(self):
        query = " UPDATE users SET show_location = %s WHERE username = %s;"
        variables = (self.show_location_status, self.username,)
        QueryHandler.execute(query, variables)

    def post(self):
        response = {}
        self.username = str(self.get_argument('username'))
        self.show_location_status = str(self.get_argument('show_location_status'))
        if not self.show_location_status in ["f", "a", "n"]:
            raise BadInfoSuppliedError("location_status")
        self.set_location_privacy()
        response["info"], response["status"] = settings.SUCCESS_RESPONSE, settings.STATUS_200
        self.write(response)

class UserInterestHandler(UserApiRequestHandler):
    """
    This class creates a link between users and interests. The interests have to
    stored beforehand.

    Methods : 
        post :
            :params 
                username => username 
                interests => json array of interest  with properties {'id': 1, 'properties': 'test'} 
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
        query = "INSERT INTO users_interest (interest_id, username, properties) VALUES"\
        +   ",".join(['(%s, %s, %s)'] * len(interests)) + ";"
        variables = []
        for interest in interests:
            properties = interest['properties'] if type(interest['properties']) == dict else json.loads(interest['properties'])
            variables += [interest['id'], self.username, psycopg2.extras.Json(properties)]
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


