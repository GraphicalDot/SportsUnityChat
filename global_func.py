# TO-DO non blocking database wrapper
from boto.s3.connection import S3Connection
from boto.s3.key import Key
import psycopg2
import psycopg2.extras
import ConfigParser
import os
config = ConfigParser.ConfigParser()
config.read(os.path.join(os.path.abspath(os.path.dirname(__file__)), 'config.py'))

class QueryHandler:
    @classmethod
    def get_connection(cls):
        connection = psycopg2.connect("dbname=%s host=%s user=%s password=%s"
                                      % (config.get('database', 'database'),
                                         config.get('database', 'host'),
                                         config.get('database', 'user'),
                                         config.get('database', 'password'))
        )
        return connection

    @classmethod
    def get_results(cls, query, variables=None):
        connection = cls.get_connection()
        cursor = connection.cursor(cursor_factory=psycopg2.extras.RealDictCursor)
        print(cursor.mogrify(query, variables))
        cursor.execute(query, variables)
        results = cursor.fetchall()
        connection.commit()
        cursor.close()
        return results

    @classmethod
    def execute(cls, query, variables=None):
        connection = cls.get_connection()
        cursor = connection.cursor()
        print(cursor.mogrify(query, variables))
        cursor.execute(query, variables)
        connection.commit()
        cursor.close()


class S3Handler:
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


def merge_dicts(dict_list):
    '''Given two dicts, merge them into a new dict as a shallow copy.'''
    z = dict_list[0].copy()
    for x in range(1, len(dict_list)):
        z.update(dict_list[x])
    return z
