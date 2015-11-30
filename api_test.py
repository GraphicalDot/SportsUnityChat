from ConfigParser import ConfigParser
import hashlib
import os
import time
from global_func import QueryHandler, S3Handler
from tornado.testing import AsyncHTTPTestCase
import json
import psycopg2
import psycopg2.extras
import unittest
import requests
import sys

import api_v0_archive
config = ConfigParser()
config.read('config.py')


class UserTest(unittest.TestCase):
    def test_user_authentication(self):
        username = "test"
        password = "password"

        query = " INSERT INTO users (username, password) VALUES (%s,%s);"
        variables = (username, password,)
        QueryHandler.execute(query, variables)

        user = api_v0_archive.User(username, password)
        user_exists = user.authenticate()

        fraud_password = 'test'
        fraud_user = api_v0_archive.User(username, fraud_password)
        user_not_exists = fraud_user.authenticate()

        query = " DELETE FROM users WHERE username = %s;"
        variables = (username, )
        QueryHandler.execute(query, variables)

        assert user_exists
        assert not user_not_exists


class RegistrationTest(AsyncHTTPTestCase):
    _phone_number = config.get('tests', 'test_phone_number')
    _auth_code = 'ASDFG'
    _registration_url = "/register?phone_number=" + str(_phone_number)
    _creation_url = "/create?phone_number=" + str(_phone_number) \
                    + "&auth_code=" + str(_auth_code)

    def get_app(self):
        return api_v0_archive.make_app()

    def test_user_registration(self):
        self.http_client.fetch(self.get_url(self._registration_url), self.stop)
        response = self.wait(timeout = 20)
        username = self._phone_number + config.get('xmpp', 'domain')
        query = " SELECT * FROM registered_users WHERE username = %s "
        variables = (username,)
        record = QueryHandler.get_results(query, variables)
        assert record
        self.assertEqual(200, json.loads(response.body)['status'])

    def test_wrong_auth_code_failure(self):
        username = self._phone_number + config.get('xmpp','domain')
        query = " UPDATE registered_users SET authorization_code = '12345'" \
                " WHERE username = %s; "
        variables = (username,)
        QueryHandler.execute(query, variables)

        self.http_client.fetch(self.get_url(self._creation_url), self.stop)
        response = self.wait(timeout=20)

        query = " SELECT * FROM users WHERE username = %s; "
        variables = (self._phone_number,)
        record = QueryHandler.get_results(query, variables)

        self.assertNotEqual(json.loads(response.body)['status'], 200)
        self.assertEqual(json.loads(response.body)['password'], None)

    def test_user_creation(self):
        username = self._phone_number + config.get('xmpp', 'domain')

        query = " DELETE FROM users WHERE username = %s;"
        variables = (self._phone_number,)
        QueryHandler.execute(query, variables)

        expiration_time = int(time.time()) + int(config.get('registration', 'expiry_period_sec'))
        query = " INSERT INTO registered_users (username, authorization_code, expiration_time) VALUES ( %s, %s, %s); "
        variables = (username, self._auth_code, expiration_time)
        QueryHandler.execute(query, variables)

        self.http_client.fetch(self.get_url(self._creation_url), self.stop)
        response = self.wait(timeout=20)

        query = " SELECT * FROM users WHERE username = %s; "
        variables = (self._phone_number, )
        record = QueryHandler.get_results(query, variables)
        self.assertEqual(json.loads(response.body)['status'], 200)
        self.assertEqual(str(username), record[0]['username']+config.get('xmpp','domain'))
        self.assertEqual(json.loads(response.body)['password'], record[0]['password'])

        query = " SELECT * FROM registered_users WHERE username = %s; "
        variables = (username,)
        record = QueryHandler.get_results(query, variables)
        self.assertEqual(len(record), 0)


class FacebookFriendServiceTest(AsyncHTTPTestCase):

    _facebook_id = config.get('tests', 'test_facebook_id')
    _id = config.get('tests', 'test_phone_number') + '@mm.io'
    _token = config.get('database','facebook_token')
    _get_facebook_friends = '/fb_friends?fb_id=' + str(_facebook_id) + '&token=' + str(_token) + \
                            '&id=' + config.get('tests', 'test_phone_number') + '@mm.io'

    def test_fb_graph_api(self):
        self.http_client.fetch(self.get_url(self._get_facebook_friends), self.stop)
        response = self.wait(timeout = 20)
        self.assertEqual(200, json.loads(response.body)['status'])

    def test_fb_id_storage(self):
        try:
            self.username = config.get('tests', 'test_phone_number')
            query = "DELETE FROM users WHERE username = %s;"
            variables = (self.username,)
            QueryHandler.execute(query, variables)
            self.password = 'password'
            query = "INSERT INTO users (username, password) VALUES (%s, %s);"
            variables = (self.username, self.password,)
            QueryHandler.execute(query, variables)
        except psycopg2.IntegrityError:
            pass

        self.http_client.fetch(self.get_url(self._get_facebook_friends), self.stop)
        self.wait(timeout = 20)
        query = " SELECT * FROM users WHERE fb_id = %s ;"
        results = QueryHandler.get_results(query, (self._facebook_id, ))
        self.assertEqual(results[0]['username'], str.split(self._id,'@')[0])

    def get_app(self):
        return api_v0_archive.make_app()


class ProfilePicServiceTest(AsyncHTTPTestCase):

    def setUp(self):
        try:
            self.username = 'test'
            self.password = 'password'
            query = "INSERT INTO users (username, password, fb_id) VALUES (%s, %s, %s);"
            variables = (self.username, self.password, config.get('tests', 'test_facebook_id'))
            QueryHandler.execute(query, variables)
        except psycopg2.IntegrityError:
            pass

    def get_app(self):
        return api_v0_archive.make_app()

    def test_profile_pic_updation(self):
        file_name = sys.argv[0]

        file_data = {'file': open(file_name, 'rb')}
        data = {
        'username': self.username,
        'password': self.password
        }
        response = requests.post(config.get('tests', 'profile_pic_url'), data=data, files=file_data)
        self.assertEqual(json.loads(response.text)['status'], 200)

        file_data = {'file': open(file_name, 'rb')}
        data = {
        'username': self.username,
        'password': 'password1'
        }
        response = requests.post(config.get('tests', 'profile_pic_url'), data=data, files=file_data)
        self.assertNotEqual(json.loads(response.text)['status'], 200)

        profile_pic_bucket = config.get('amazon', 'profile_pics_bucket')
        S3Handler(profile_pic_bucket)

        file_name = self.username
        s3_file_url = "https://%s.s3.amazonaws.com/%s" % (profile_pic_bucket, file_name)
        s3_file_response = requests.get(s3_file_url)

        self.assertEqual(200, s3_file_response.status_code)

    def tearDown(self):
        query = "DELETE FROM users WHERE username = %s;"
        variables = (self.username,)
        QueryHandler.execute(query, variables)

class MediaTest(AsyncHTTPTestCase):
    def getUp(self):
        file_storage_name = "media/md5_sample"
        if os.path.isfile(file_storage_name):
            os.remove(file_storage_name)


    def test_upload_and_download(self):
        from IPython import embed
        self.url = "http://localhost:3000/media"
        file_name = sys.argv[0]
        import base64
        file_content = open(file_name, 'r').read()
        md5 = hashlib.md5(file_content).hexdigest()
        headers = {'Checksum': 'md5_sample'}
        response = requests.post(self.url, headers=headers, data=file_content)
        assert json.loads(response.content)['status'] == 200
        assert os.path.isfile('media/md5_sample')

        self.url = "http://localhost:3000/media?name=md5_sample"
        response = requests.get(self.url)
        from IPython import embed
        assert response.content
        assert file_content == response.content

    def get_app(self):
        return api_v0_archive.make_app()

    def tearDown(self):
        pass

if __name__ == '__main__':
    unittest.main()
