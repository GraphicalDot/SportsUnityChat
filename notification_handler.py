from global_func import Singleton, QueryHandler
import threading
import ConfigParser
config = ConfigParser.ConfigParser()
config.read(os.path.join(os.path.abspath(os.path.dirname(__file__)), 'config.py'))
class ApnsHandler:
    __metaclass__ = Singleton
    def __init__(self):
        cert_file = config.read('apns', 'cert_file')
        key_file = config.read('apns', 'key_file')
        self.apns = APNs(use_sandbox=True, cert_file=cert_file, key_file=key_file)

    def send_notifications(self, users, event):
        frame = Frame()
        payload = Payload(alert = str(event), sound="default", badge=1)
        for idx, user in enumerate(users):
            if user['apple_token']:
                identifier = idx + 1
                expiry = time.time() + 3600
                priority = 10
                frame.add_item(user['apple_token'], payload, identifier, expiry, priority)
        self.apns.gateway_server.send_notification_multiple(frame)

class GCMHandler:
    __metaclass__ = Singleton
    def __init__(self):
        api_key = config.read('gcm', 'api_key')
        self.gcm = GCM(api_key)
        
    def send_notifications(self, users, event):
        users_tokens = []
        for user in users:
            if users['android_token']: users_tokens.append(users['android_token'])
        response = self.gcm.json_request(registration_ids = users_tokens, data=event)

class NotificationHandler(self):
    def __init__(self, match_id, event):
        threading.Thread(group = None, target = self.handle_notification, name = None, args = (match_id, event)).start()
    
    def get_subscribing_users(self, match_id):
        query = " SELECT android_token, apple_token FROM users WHERE users_matches.match_id = %s AND users_matches.username = users.username;"
        variables = (match_id,)
        return QueryHandler.get_results(query, variables)

    def handle_notifcation(self, match_id, event):
        subscribing_users = self.get_subscribing_users(self ,match_id)
        ApnsHandler().send_notifications(subscribing_users, event)
        GCMHandler().send_notifications(subscribing_users, event)