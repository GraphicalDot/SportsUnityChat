# from redis import Redis
# from rq import Queue
import psycopg2.extras
import json
import settings
import os
from global_func import QueryHandler
import time
import copy
import threading
import ConfigParser
config = ConfigParser.ConfigParser()
config.read(os.path.join(os.path.abspath(os.path.dirname(__file__)), 'config.py'))
import apns
from gcm import GCM

class ApnsHandler(object):
    _instance = None

    ## __new__ has been overridden to implement a singleton pattern 
    def __new__(cls, *args, **kwargs):
        if not cls._instance:
            cls._instance = super(ApnsHandler, cls).__new__(cls, *args, **kwargs)
        return cls._instance

    def __init__(self):
        cert_file = config.get('apns', 'cert_file')
        key_file = config.get('apns', 'key_file')
        self.apns = apns.APNs(cert_file=cert_file, key_file=key_file, enhanced=True)

    def send_notifications(self, users, event):
        payload = copy.deepcopy(event)
        frame = apns.Frame()
        top_text = payload["tt"]
        payload.pop("tt", None)
        bottom_text = payload["bt"]
        payload.pop("bt", None)
        alert = {"title": top_text, "body": bottom_text}
        payload = apns.Payload(alert = alert, badge=1, sound = "default", custom=payload)
        for idx, user in enumerate(users):
            if user['token_type'] == settings.TOKEN_IOS_TYPE:
                identifier = idx + 1
                expiry = time.time() + 3600
                priority = 10
                frame.add_item(user['device_token'], payload, identifier, expiry, priority)
        response = self.apns.gateway_server.send_notification_multiple(frame)
        return response

class GCMHandler:
    def __init__(self):
        api_key = config.get('gcm', 'api_key')
        self.gcm = GCM(api_key)
        
    def send_notifications(self, users, event):
        payload = {"data": {"message": event}}
        users_tokens = []
        for user in users:
            if user['token_type'] == settings.TOKEN_ANDROID_TYPE: 
                users_tokens.append(user['device_token'])
        if users_tokens:
            response = self.gcm.json_request(registration_ids = users_tokens, data=payload)
            return response

    def handle_response(self, response):
        print response
        #TO-DO Handle response from gcm
        pass

class NotificationHandler:
    def __init__(self, match_id, payload):
        self.match_id = match_id
        self.payload = payload
    
    def notify(self):
        # threading.Thread(group = None, target = self.handle_notification, name = None, args = ()).start()
        self.handle_notification()

    def get_subscribing_users(self, match_id):
        query = " SELECT device_token, token_type FROM users, users_matches"\
        + " WHERE users_matches.match_id = %s AND users_matches.username = users.username;"
        variables = (match_id,)
        return QueryHandler.get_results(query, variables)

    def handle_notification(self):
        subscribing_users = self.get_subscribing_users(self.match_id)
        try:
            self.apns_response = ApnsHandler().send_notifications(subscribing_users, self.payload)
            self.gcm_response = GCMHandler().send_notifications(subscribing_users, self.payload)
            self.sending_error = None
        except Exception, e:
            self.sending_error = e.message
        self.handle_responses()

    def handle_responses(self):
        query = " INSERT INTO notifications (match_id, notification, apns_response, gcm_response, error) VALUES (%s, %s, %s, %s, %s);"
        variables = (self.match_id, psycopg2.extras.Json(self.payload), str(self.apns_response), psycopg2.extras.Json(self.gcm_response), str(self.sending_error))
        QueryHandler().execute(query, variables)
