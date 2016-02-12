import hashlib
import magic
import os
import requests
import time
from ConfigParser import ConfigParser
from IPython import embed
from nose.tools import assert_equal

from global_func import QueryHandler, S3Handler
from requests_toolbelt.multipart.encoder import MultipartEncoder
from tornado.testing import AsyncHTTPTestCase
import json
import psycopg2
import psycopg2.extras
import unittest
import requests
import sys
from requests_toolbelt import MultipartEncoder
from custom_error import BadAuthentication

import api_v0_archive
config = ConfigParser()
config.read('config.py')

def create_user(username, password, phone_number):
    query = " INSERT INTO users (username, password, phone_number) VALUES (%s,%s, %s);"
    variables = (username, password, phone_number,)
    QueryHandler.execute(query, variables)

def delete_user(username = None, phone_number = None):
    if username or phone_number:
        query = " DELETE FROM users WHERE " + (" phone_number " if phone_number else " username ") + "= %s;"  
        variables = (phone_number if phone_number else username, )
        QueryHandler.execute(query, variables)
    else:
        raise Exception 

def select_user(username = None, phone_number = None):
    if username or phone_number:
        query = " SELECT * FROM users WHERE " + (" phone_number " if phone_number else " username ") + "= %s;"  
        variables = (phone_number if phone_number else username, )
        return QueryHandler.get_results(query, variables)
    else:
        raise Exception 

class UserTest(unittest.TestCase):

    def test_user_authentication(self):
        phone_number = config.get('tests', 'test_phone_number')
        username = "test"
        password = "password"

        delete_user(username = username)
        create_user(username, password, phone_number)

        user = api_v0_archive.User(username = username, password = password)

        fraud_password = 'test'
        fraud_user = api_v0_archive.User(username, fraud_password)
        try:
            user.authenticate()
            fraud_user.authenticate() 
        except BadAuthentication:
            pass
        except Exception, e:
            raise AssertionError



class CreationTest(AsyncHTTPTestCase):
    _phone_number = config.get('tests', 'test_phone_number')
    _username = 'test'
    _auth_code = 'ASDFG'
    _registration_url = "/register?phone_number=" + str(_phone_number)
    _creation_url = "/create?phone_number=" + str(_phone_number) \
                    + "&auth_code=" + str(_auth_code)

    def setUp(self):
        super(CreationTest, self).setUp()
        delete_user(username = self._username)

    def get_app(self):
        return api_v0_archive.make_app()

    # def test_user_registration(self):
    #     self.http_client.fetch(self.get_url(self._registration_url), self.stop)
    #     response = self.wait(timeout=20)
    #     query = " SELECT * FROM registered_users WHERE phone_number = %s "
    #     variables = (self._phone_number,)
    #     record = QueryHandler.get_results(query, variables)
    #     assert record
    #     self.assertEqual(200, json.loads(response.body)['status'])

    def test_wrong_auth_code_failure(self):
        query = " UPDATE registered_users SET authorization_code = '12345' " \
                " WHERE phone_number = %s; "
        variables = (self._phone_number,)
        QueryHandler.execute(query, variables)

        self.http_client.fetch(self.get_url(self._creation_url), self.stop)
        response = self.wait(timeout=20)

        assert json.loads(response.body).get('password' ,None) == None
        self.assertNotEqual(json.loads(response.body)['status'], 200)

        assert not select_user(self._phone_number)


    def test_user_creation(self):

        delete_user(username = self._username)

        query = " DELETE FROM registered_users WHERE phone_number = %s; "
        variables = (self._phone_number,)
        QueryHandler.execute(query, variables)


        expiration_time = int(time.time()) + int(config.get('registration', 'expiry_period_sec'))
        query = " INSERT INTO registered_users (authorization_code, expiration_time, phone_number) VALUES ( %s, %s, %s); "
        variables = (self._auth_code, expiration_time, self._phone_number)
        QueryHandler.execute(query, variables)

        self.http_client.fetch(self.get_url(self._creation_url), self.stop)
        response = self.wait(timeout=20)

        record = select_user(phone_number = self._phone_number)
        
        self.assertEqual(json.loads(response.body)['status'], 200)
        assert record[0]['username']
        self.assertEqual(
            json.loads(response.body)['username'], record[0]['username'])
        old_username = record[0]['username']
        
        self.assertEqual(
            json.loads(response.body)['password'], record[0]['password'])

        expiration_time = int(time.time()) + int(config.get('registration', 'expiry_period_sec'))
        query = " INSERT INTO registered_users (authorization_code, expiration_time, phone_number) VALUES ( %s, %s, %s); "
        variables = (self._auth_code, expiration_time, self._phone_number)
        QueryHandler.execute(query, variables)        

        self.http_client.fetch(self.get_url(self._creation_url), self.stop)
        response = self.wait(timeout=20)

        self.assertEqual(
            json.loads(response.body)['username'], old_username)

        query = " SELECT * FROM registered_users WHERE phone_number = %s; "
        variables = (self._phone_number,)
        record = QueryHandler.get_results(query, variables)
        assert not record


# class FacebookFriendServiceTest(AsyncHTTPTestCase):

#     _facebook_id = config.get('tests', 'test_facebook_id')
#     _id = config.get('tests', 'test_phone_number') + '@mm.io'
#     _token = config.get('database', 'facebook_token')
#     _get_facebook_friends = '/fb_friends?fb_id=' + str(_facebook_id) + '&token=' + str(_token) + \
#                             '&id=' + \
#         config.get('tests', 'test_phone_number') + '@mm.io'

#     def test_fb_graph_api(self):
#         self.http_client.fetch(
#             self.get_url(self._get_facebook_friends), self.stop)
#         response = self.wait(timeout=20)
#         self.assertEqual(200, json.loads(response.body)['status'])

#     def test_fb_id_storage(self):
#         try:
#             self.phone_number = config.get('tests', 'test_phone_number')
#             self.username = 'test'
#             self.password = 'password'

#             delete_user(username = self.username)

#             query = "INSERT INTO users (username, password) VALUES (%s, %s);"
#             variables = (self.username, self.password,)
#             QueryHandler.execute(query, variables)
#         except psycopg2.IntegrityError:
#             pass

#         self.http_client.fetch(
#             self.get_url(self._get_facebook_friends), self.stop)
#         self.wait(timeout=20)
        
#         query = " SELECT * FROM users WHERE fb_id = %s ;"
#         results = QueryHandler.get_results(query, (self._facebook_id, ))
#         self.assertEqual(results[0]['username'], str.split(self._id, '@')[0])

#     def get_app(self):
#         return api_v0_archive.make_app()


# class ProfilePicServiceTest(AsyncHTTPTestCase):
#     _username = 'test'
#     _password = 'password'
#     _phone_number = config.get('tests', 'test_phone_number')

#     def setUp(self):
#         super(ProfilePicServiceTest, self).setUp()
#         try:
#             create_user(username = self._username, password = self._password, phone_number = self._phone_number)
            
#             query = "INSERT INTO users (fb_id) VALUES (%s) WHERE username = %s;"
#             variables = (self._username, config.get('tests', 'test_facebook_id'))
#             QueryHandler.execute(query, variables)
#         except psycopg2.IntegrityError:
#             pass

#     def get_app(self):
#         return api_v0_archive.make_app()

#     def test_profile_pic_updation(self):
#         file_name = sys.argv[0]

#         file_data = {'file': open(file_name, 'rb')}
#         data = {
#             'username': self._username,
#             'password': self._password
#         }
#         response = requests.post(
#             config.get('tests', 'profile_pic_url'), data=data, files=file_data)
#         self.assertEqual(json.loads(response.text)['status'], 200)

#         file_data = {'file': open(file_name, 'rb')}
#         data = {
#             'username': self._username,
#             'password': 'password1'
#         }
#         response = requests.post(
#             config.get('tests', 'profile_pic_url'), data=data, files=file_data)
#         self.assertNotEqual(json.loads(response.text)['status'], 200)

#         profile_pic_bucket = config.get('amazon', 'profile_pics_bucket')
#         S3Handler(profile_pic_bucket)

#         file_name = self._username
#         s3_file_url = "https://%s.s3.amazonaws.com/%s" % (
#             profile_pic_bucket, file_name)
#         s3_file_response = requests.get(s3_file_url)

#         self.assertEqual(200, s3_file_response.status_code)

#     def tearDown(self):
#         delete_user(username = self._username)


class LocationTest(AsyncHTTPTestCase):
    _username = 'test'
    _password = 'password'
    _phone_number = config.get('tests', 'test_phone_number')

    def setUp(self):
        super(LocationTest, self).setUp()
        try:
            delete_user(username = self._username)
            create_user(username = self._username, password = self._phone_number, phone_number = self._phone_number)
        except psycopg2.IntegrityError:
            pass

        try:
            interests = ['interest_one', 'interest_two']
            query = " DELETE FROM interest WHERE "\
            + " OR ".join(map( lambda interest: "interest_name = '" + interest + "'" , interests))\
            + " ;"
            QueryHandler.execute(query, ())

            query = "INSERT INTO interest (interest_name) VALUES ('interest_one'), ('interest_two');"
            QueryHandler.execute(query, ())
        except psycopg2.IntegrityError, e:
            pass

        query = "INSERT INTO users_interest (interest_id, username) "\
        + " (SELECT interest_id, %s FROM interest WHERE "\
        + " OR ".join(map( lambda interest: "interest_name = '" + interest + "'" , interests))\
        + ");"

        variables = (self._username, )
        try:
            QueryHandler.execute(query, variables)
        except psycopg2.IntegrityError, e:
            pass

    def get_app(self):
        return api_v0_archive.make_app()

    def test_storage(self):
        lat = "0.0"
        lng = "0.0"
        self.set_location_storage_url = "/set_location?"\
            + "lat=" + lat \
            + "&lng=" + lng \
            + "&user=" + self._username
        self.http_client.fetch(
            self.get_url(self.set_location_storage_url), self.stop)
        response = self.wait(timeout=20)

        assert json.loads(response.body)['status'] == 200




        query = " SELECT lat, lng FROM users WHERE username = %s;"
        variables = (self._username,)
        result = QueryHandler.get_results(query, variables)
        assert str(result[0]['lat']) == lat
        assert str(result[0]['lng']) == lng

    def test_retrieval(self):
        lat = "0.0"
        lng = "0.0"
        radius = "5"
        self.username = config.get('tests', 'test_phone_number')
        self.test_storage()

        nearby_user = "a"
        nearby_user_password = "password"
        nearby_user_phone = "00000000"

        try:
            create_user(username = nearby_user, password = nearby_user_password, phone_number = nearby_user_phone)
            query = " UPDATE users SET is_available = True WHERE username = %s;"
            variables = (nearby_user,)
            QueryHandler.execute(query, variables)
        except psycopg2.IntegrityError:
            pass

        interests = ['interest_one', 'interest_two']
        test_storage_url = "/set_user_interests?username=" + nearby_user\
            + "".join(map(lambda interest: "&interests=" + interest, interests))
        self.http_client.fetch(
            self.get_url(test_storage_url), self.stop)
        response = self.wait(timeout=20)

        nearby_user_lat = "0.0000009"
        nearby_user_lng = "0.0000009"
        self.set_location_storage_url = "/set_location?"\
            + "lat=" + nearby_user_lat \
            + "&lng=" + nearby_user_lng \
            + "&user=" + nearby_user


        self.http_client.fetch(
            self.get_url(self.set_location_storage_url), self.stop)
        response = self.wait(timeout=20)


        retrirval_url = "/retrieve_nearby_users?"\
            + "lat=" +  lat\
            + "&lng=" + lng\
            + "&radius=" + radius

        self.http_client.fetch(
            self.get_url(retrirval_url), self.stop)
        response = self.wait(timeout=20)

        delete_user(username = nearby_user)

        print response.body
        assert response
        assert json.loads(response.body)['status'] == 200
        assert json.loads(response.body)['users']
        assert type(json.loads(response.body)['users']) == list
        assert json.loads(response.body)['users'][0]['username']
        assert type(json.loads(response.body)['users'][0]['distance']) == float
        assert type(json.loads(response.body)['users'][0]['lat']) == float
        assert type(json.loads(response.body)['users'][0]['lng']) == float
        assert json.loads(response.body)['users'][0]['interests']

    def tearDown(self):
        pass

class InterestTest(AsyncHTTPTestCase):
    _username = 'test'
    _password = 'password'
    _phone_number = config.get('tests', 'test_phone_number')

    def get_app(self):
        return api_v0_archive.make_app()

    def setUp(self):
        super(InterestTest, self).setUp()
        try:
            delete_user(username = self._username)
            create_user(username = self._username, password = self._phone_number, phone_number = self._phone_number)

            query = " DELETE FROM users_interest WHERE username = %s;"
            variables = (self._username,)
            QueryHandler.execute(query, variables)
        except psycopg2.IntegrityError:
            pass

        try:
            interests = ['interest_one', 'interest_two']
            query = " DELETE FROM interest WHERE "\
                " interest_name = 'interest_one' OR interest_name = 'interest_two';"
            variables = ()
            QueryHandler.execute(query, variables)

            interests = ['interest_one', 'interest_two']
            query = "INSERT INTO interest (interest_name) VALUES ('interest_one'), ('interest_two');"
            QueryHandler.execute(query, ())
        except psycopg2.IntegrityError, e:
            pass


    def test_storage(self):
        interests = ['interest_one', 'interest_two']

        test_storage_url = "/set_user_interests?username=" + self._username\
            + "".join(map(lambda interest: "&interests=" + interest, interests))

        self.http_client.fetch(
            self.get_url(test_storage_url), self.stop)
        response = self.wait(timeout=20)

        assert response
        assert json.loads(response.body)['status'] == 200


        query = "select users.username, string_agg(interest.interest_name, ' ,') as interests from users "\
            + " left outer join users_interest on (users.username = users_interest.username) "\
            + " left outer join interest on (users_interest.interest_id = interest.interest_id)"\
            + " WHERE users.username = %s group by users.username;"
        variables = (self._username,)
        record = QueryHandler.get_results(query, variables)

        assert record
        assert record[0]['username']
        assert record[0]['interests'] == "interest_one ,interest_two"

        interests = ['interest_one']
        test_storage_url = "/set_user_interests?username=" + self._username\
            + "".join(map(lambda interest: "&interests=" + interest, interests))

        self.http_client.fetch(
            self.get_url(test_storage_url), self.stop)
        response = self.wait(timeout=20)

        assert response
        assert json.loads(response.body)['status'] == 200

        query = "select users.username, string_agg(interest.interest_name, ' ,') as interests from users "\
            + " left outer join users_interest on (users.username = users_interest.username) "\
            + " left outer join interest on (users_interest.interest_id = interest.interest_id)"\
            + " WHERE users.username = %s group by users.username;"
        variables = (self._username,)
        record = QueryHandler.get_results(query, variables)

        assert record
        assert record[0]['username']
        assert record[0]['interests'] == "interest_one"

    def tearDown(self):
        pass


class MediaTest(AsyncHTTPTestCase):

    def setUp(self):
        super(MediaTest, self).setUp()
        file_storage_name = "media/md5_sample"
        file_storage_name2 = "media/big.mp4"
        if os.path.isfile(file_storage_name):
            os.remove(file_storage_name)
        if os.path.isfile(file_storage_name2):
            os.remove(file_storage_name2)

    def test_upload_download_media_presence(self):
        self.url = "http://localhost:3000/media"
        self.media_presence_url = "http://localhost:3000/media_present?name=md5_sample"
        file_name = sys.argv[0]
        file_content = open(file_name, 'r').read()
        md5 = hashlib.md5(file_content).hexdigest()
        headers = {'Checksum': 'md5_sample'}
        response = requests.post(self.url, headers=headers, data=file_content)
        assert json.loads(response.content)['status'] == 200
        assert os.path.isfile('media/md5_sample')

        self.url = "http://localhost:3000/media?name=md5_sample"
        response = requests.get(self.url)
        assert response.content

        response = requests.get(self.media_presence_url)
        assert json.loads(response.content)["status"] == 200

        file_storage_name = "media/md5_sample"
        os.remove(file_storage_name)

        response = requests.get(self.media_presence_url)
        assert json.loads(response.content)["status"] == 400

        self.url = "http://localhost:3000/media"
        file_name2 = 'big.mp4'
        headers = {'Checksum': 'big.mp4'}
        with open(file_name2, 'rb') as file_content2:
            response = requests.post(
                self.url, headers=headers, data=file_content2)
        assert json.loads(response.content)['status'] == 200
        assert os.path.isfile('media/big.mp4')

        self.url = "http://localhost:3000/media?name=big.mp4"
        response = requests.get(self.url)
        assert response.content
        assert response.content == open(file_name2, 'rb').read()

    def get_app(self):
        return api_v0_archive.make_app()

    def tearDown(self):
        pass


class IOSMediaHandlerTests(unittest.TestCase):
    url = None
    filename = None

    def setUp(self):
        self.url = 'http://localhost:3000/media_multipart'
        self.test_files = ['big.mp4']

    def test_validations(self):
        self.filename = self.test_files[0]
        mime = magic.Magic(mime=True)
        mime_type = mime.from_file(self.filename)
        encoder = MultipartEncoder(
            fields={'name': 'image', 'filename': self.filename, 'Content-Disposition': 'form-data',
                    'Content-Type': mime_type, 'file': (self.filename, open(self.filename, 'rb'), mime_type)}
        )

        # 'Content-type' not provided
        response = requests.post(self.url, data=encoder.to_string(), headers={'Checksum': 'test_image'})
        res = json.loads(response.content)
        assert response.status_code == 200
        assert res['info'] == " Bad Request: 'Content-Type' field not present in the Header!"
        assert res['status'] == 400

        # 'Checksum' not provided
        response = requests.post(self.url, data=encoder.to_string(), headers={'Content-Type': encoder.content_type})
        res = json.loads(response.content)
        assert response.status_code == 200
        assert res['info'] == " Bad Request: 'Checksum' field not present in the Header!"
        assert res['status'] == 400

        # Request body not provided
        response = requests.post(self.url, headers={'Checksum': 'test_image', 'Content-Type': encoder.content_type})
        res = json.loads(response.content)
        assert response.status_code == 200
        assert res['info'] == " Bad request: Request body not present!"
        assert res['status'] == 400

    def test_upload_media(self):

        # test on different media files
        for file in self.test_files:
            self.filename = file
            mime = magic.Magic(mime=True)
            mime_type = mime.from_file(self.filename)
            encoder = MultipartEncoder(
                fields={'name': 'image', 'filename': self.filename, 'Content-Disposition': 'form-data',
                        'Content-Type': mime_type, 'file': (self.filename, open(self.filename, 'rb'), mime_type)}
                )
            response = requests.post(self.url, data=encoder.to_string(),
                                     headers={'Content-Type': encoder.content_type, 'Checksum': file})
            res = json.loads(response.content)
            assert response.status_code == 200
            assert res['info'] == 'Success'
            assert res['status'] == 200


class IOSSetUserDeviceIdTests(unittest.TestCase):

    _username = 'test'
    _password = 'password'
    _phone_number = config.get('tests', 'test_phone_number')

    def assert_status_info(self, response, expected_status):
        res = json.loads(response.content)
        assert_equal(response.status_code, 200)
        assert_equal(json.loads(response.content)['status'], expected_status)

    def setUp(self):
        self.url = 'http://localhost:3000/set_udid'
        delete_user(username = self._username)
        create_user(username = self._username, password = self._password, phone_number = self._phone_number)

    def test_validations(self):
        # self._username not provided
        self.data = {'token': 'AAAAAAAA'}
        response = requests.post(self.url, data=self.data)
        self.assert_status_info(response, 400)

        # udid token not provided
        self.data = {'user': self._username}
        response = requests.post(self.url, data=self.data)
        self.assert_status_info(response, 400)

        # self._username is not registered
        self.data = {'user': '910000000000', 'token': 'AAAAAA'}
        response = requests.post(self.url, data=self.data)
        self.assert_status_info(response, 400)

    def test_post(self):
        self.data = {'user': self._username, 'token': 'AAAAAA', 'password': self._password}
        response = requests.post(self.url, data=self.data)
        self.assert_status_info(response, 200)


if __name__ == '__main__':
    unittest.main()
