# TO-DO non blocking database wrapper
import requests
import settings
from boto.s3.connection import S3Connection
from boto.s3.key import Key
import psycopg2
import psycopg2.extras
from apns import APNs, Payload
import os
import json
from gcm import GCM
import time
import ConfigParser
config = ConfigParser.ConfigParser()
config.read(os.path.dirname(__file__) + '/../config.py')
class QueryHandler(object):

    _instance = None
    def __new__(cls, *args, **kwargs):
        if not cls._instance:
            cls._instance = super(QueryHandler, cls).__new__(cls, *args, **kwargs)
        return cls._instance

    def __init__(self):
        self.start_connection()

    def get_connection(self):
        return self.connection

    def start_connection(self):
        connection = psycopg2.connect("dbname=%s host=%s user=%s password=%s"
                                      % (config.get('database', 'database'),
                                         config.get('database', 'host'),
                                         config.get('database', 'user'),
                                         config.get('database', 'password'))
        )
        self.connection = connection

    @classmethod
    def get_results(cls, query, variables=None):
        connection = QueryHandler().get_connection()
        cursor = connection.cursor(cursor_factory=psycopg2.extras.RealDictCursor)
        print(cursor.mogrify(query, variables))
        cursor.execute(query, variables)
        results = cursor.fetchall()
        connection.commit()
        cursor.close()
        return results

    @classmethod
    def execute(cls, query, variables=None):
        connection = QueryHandler().get_connection()
        cursor = connection.cursor()
        print(cursor.mogrify(query, variables))
        cursor.execute(query, variables)
        connection.commit()
        cursor.close()


class S3(object):
    def __init__(self, bucket_name):
        amazon_access_key = str.strip(str(config.get('amazon', 'amazon_access_key')))
        amazon_secret_key = str.strip(str(config.get('amazon', 'amazon_secret_key')))
        connection = S3Connection(amazon_access_key, amazon_secret_key)
        self.bucket = connection.get_bucket(bucket_name, validate=False)
        self.key = Key(self.bucket)

    def upload(self, key, file):
        self.key.key = key
        file_size = self.key.set_contents_from_string(file)
        acl = str.strip(str(config.get('amazon', 'acl')))
        self.key.set_acl(acl)
        return file_size

    def check_exists(self, key):
        return self.bucket.get_key(key)

    def delete_key(self, key):
        return self.bucket.delete_key(key)

    def get_file(self, key):
        key = self.check_exists(key)
        if key:
            return key.get_contents_as_string()
        else:
            return None

def merge_dicts(dict_list):
    '''Given two dicts, merge them into a new dict as a shallow copy.'''
    z = dict_list[0].copy()
    for x in range(1, len(dict_list)):
        z.update(dict_list[x])
    return z

def merge_body_arguments(request_handler_object):
    try:
        body_argument = json.loads(request_handler_object.request.body)
        listing_function = lambda x: x if type(x) == list else [str(x)]
        body_argument = {k: listing_function(v) for k, v in body_argument.iteritems()}
        request_handler_object.request.arguments.update(body_argument)
        return request_handler_object.request.arguments
    except ValueError:
        return request_handler_object.request.arguments


def send_message(number, message):
    """
    Sends sms on a mobile number.
    """
    payload = {
        'method': 'sms',
        'api_key': settings.SINFINI_API_KEY,
        'message': message.strip(),
        'sender': settings.SINFINI_SENDER_ID,
        'to': str.strip(number),
        'format': 'json',
        'custom': '1,2',
        'flash': '0'
    }
    response = requests.get(settings.SINFINI_MESSAGE_GATEWAY, params=payload)
    json_response = response.json()
    return (settings.SUCCESS_RESPONSE, settings.STATUS_200, json_response) if json_response['status'] == 'OK' \
        else (json_response['message'], settings.STATUS_500)

def is_number(s):
    try:
        float(s)
        return True
    except ValueError:
        return False

