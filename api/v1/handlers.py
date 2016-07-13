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
import threading
from requests_toolbelt import MultipartDecoder
from common.custom_error import BadAuthentication, BadInfoSuppliedError
from sleekxmpp import ClientXMPP
from xml.etree import cElementTree as ET
import xmltodict
import dicttoxml
from sleekxmpp.plugins.base import PluginManager, PluginNotFound, BasePlugin
from sleekxmpp.plugins.base import register_plugin, load_plugin
from sleekxmpp.exceptions import IqError, IqTimeout
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



def check_udid_and_apk_version(request_handler_object):
    request_handler_object.get_argument('apk_version')
    request_handler_object.get_argument('udid')


class PubSubMessageClient(ClientXMPP):

    def __init__(self, client_jid, client_password, match_id, series_id, group_node_id):
        print 'insid init'
        self.client_jid = client_jid + '@mm.io'
        self.client_password = client_password
        self.match_id = match_id
        self.series_id = series_id
        self.group_node_id = str(group_node_id)
        ClientXMPP.__init__(self, self.client_jid, self.client_password)
        self._start_thread("get_subscriptions", self.get_subscriptions)

    def callback_function(self, *args, **kwargs):
        print 'inside callback_function'
        print 'args:', args
        print 'kwargs::', kwargs

    def get_subscriptions(self):
        print 'inside get_subscriptions'
        try:
            m = self.plugin['xep_0060']
            msg = m.get_node_subscriptions(jid=self.client_jid, node=self.group_node_id, ifrom=self.client_jid, block=True, callback=self.callback_function)
            print '####', msg
            # msg = self.plugin['xep_0060'].get_node_subscriptions(jid=self.client_jid, node=self.group_node_id, ifrom=self.client_jid, block=True)
            # print 'msg:::', msg
        except IqError as e:
            print e


    # def chat_send(self):
    #     print 'inside chat send'
    #     self.send_message(mto='pubsub.mm.io', mbody=self.match_to_subscribe_to, msubject="New Live matches", mtype='chat',
    #                       mfrom=self.client_jid)


class SubscribeToCommentary(tornado.web.RequestHandler):
    """

    """
    # def send_pubsub_msg(self, match_id, series_id, group_id):
    #     print 'inside send_pubsub_msg'
    #     xmpp = PubSubMessageClient(client_jid='aakarshi@mm.io', client_password='password', match_to_subscribe_to=str(series_id) + ':' + str(match_id) + '-' + str(group_id))
    #     print "xmpp::", xmpp
    #     # start separate thread
    #     if xmpp.connect(('localhost', 5222)):
    #         xmpp.process(block=True)
    #         print("Done")
    #     else:
    #         print("Unable to connect.")

    def subscribe_group_to_match(self, match_id, series_id, group_node_id, sender, password):
        print 'inside subscribe_group_to_match'
        xmpp = PubSubMessageClient(sender, password, match_id, series_id, group_node_id)
        xmpp.register_plugin('xep_0060')
        if xmpp.connect(('localhost', 5222)):
            xmpp.process(block=True)
            print("Done")
        else:
            print("Unable to connect.")


    def get(self):
        response = {}
        try:
            check_udid_and_apk_version(self)
            print 'inside get of SubscribeToCommentary'
            match_id = str(self.get_argument('match_id'))
            series_id = str(self.get_argument('series_id'))
            group_node_id = str(self.get_argument('group_node_id'))
            username = str(self.get_argument('username'))
            password = str(self.get_argument('password'))

            # start separate thread for each group subscription
            threading.Thread(group = None, target = self.subscribe_group_to_match, name = None, args = (match_id, series_id, group_node_id, username, password)).start()


            # self.send_pubsub_msg(match_id, series_id, group_id)
        except MissingArgumentError, status:
            response["info"] = status.log_message
            response["status"] =settings.STATUS_400
        except Exception as e:
            response['status'] = settings.STATUS_500
            response['info'] = " Error is: %s" % e
        finally:
            self.write(response)

