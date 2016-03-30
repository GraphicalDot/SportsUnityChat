##TO-DO Refactor this file
import hashlib
import json
import magic
import os
import psycopg2
import psycopg2.extras
import requests
import sys
import time
import unittest
from ConfigParser import ConfigParser
from ConfigParser import ConfigParser
from global_func import QueryHandler, S3Handler, merge_dicts
from requests_toolbelt.multipart.encoder import MultipartEncoder
from tornado.testing import AsyncHTTPTestCase
from requests_toolbelt import MultipartEncoder
from shutil import copyfile
from custom_error import BadAuthentication
import api_v0_archive
from user import User
import settings
import test_utils
import copy

config = ConfigParser()
config.read('config.py')

extra_params = '&apk_version=v0.1&udid=TEST@UDID'
extra_params_dict = {'apk_version' : 'v0.1', 'udid' : "test_udid"}
tornado_listening_port =  int(config.get('tornado', 'listening_port'))
tornado_local_address =  "http://localhost:%u" % tornado_listening_port


class UserTest(unittest.TestCase):

    def test_user_authentication(self):
        phone_number = config.get('tests', 'test_phone_number')
        username = "test"
        password = "password"

        test_utils.delete_user(username = username, phone_number=phone_number)
        test_utils.create_user(username, password, phone_number)

        user = User(username = username, password = password)

        fraud_password = 'test'
        fraud_user = User(username, fraud_password)

        try:
            user.authenticate()
        except BadAuthentication:
            raise AssertionError

        try:
            fraud_user.authenticate()
        except BadAuthentication:
            pass

class CreationTest(AsyncHTTPTestCase):
    username = None
    _phone_number = config.get('tests', 'test_phone_number')
    _password = 'password'
    _username = 'test'
    _auth_code = 'ASDFG'
    _registration_url = tornado_local_address + "/register?phone_number=" + str(_phone_number)
    _creation_url = tornado_local_address + "/create?phone_number=" + str(_phone_number) \
                    + "&auth_code=" + str(_auth_code) + extra_params

    def setUp(self):
        super(CreationTest, self).setUp()
        test_utils.delete_user(username=self._username, phone_number=self._phone_number)
        test_utils.delete_registered_user(phone_number=self._phone_number)

    def get_app(self):
        return api_v0_archive.Application()

    def test_user_registration(self):

        # Invalid url params
        response = requests.get(self._registration_url)
        res = json.loads(response.text)
        self.assertEqual(res['info'], "Missing argument apk_version")
        self.assertEqual(res['status'], settings.STATUS_400)

        query = "SELECT * FROM registered_users WHERE phone_number = %s;"
        variables = (self._phone_number,)
        record = QueryHandler.get_results(query, variables)
        self.assertEqual(len(record), 0)


        # test for banned user with valid url
        query = " INSERT INTO users (username, password, phone_number, is_banned) VALUES (%s,%s, %s, True);"
        variables = (self._username, self._password, self._phone_number,)
        QueryHandler.execute(query, variables)

        response = requests.get(self._registration_url + extra_params)
        res = json.loads(response.text)
        select_query = "SELECT * FROM registered_users WHERE phone_number = %s;"
        select_variables = (self._phone_number,)
        record = QueryHandler.get_results(select_query, select_variables)

        self.assertEqual(len(record), 0)
        self.assertEqual(res['status'], settings.STATUS_403)
        self.assertEqual(res['info'], settings.USER_FORBIDDEN_ERROR)


        # test for non-banned user with valid url
        query = "UPDATE users SET is_banned=False WHERE phone_number=%s;"
        variables = (self._phone_number,)
        QueryHandler.execute(query, variables)
        response = requests.get(self._registration_url + extra_params)
        res = json.loads(response.text)
        record = QueryHandler.get_results(select_query, select_variables)
        self.assertEqual(len(record), 1)
        assert record[0]['gateway_response']
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        self.assertEqual(res['status'], settings.STATUS_200)

    def test_wrong_auth_code_failure(self):

        # first register the user
        requests.get(self._registration_url + extra_params)

        # create the user with wrong OTP
        response = requests.get(self._creation_url + extra_params)
        res = json.loads(response.text)
        self.assertEqual(res['info'], " Wrong or Expired Token ")
        self.assertEqual(res['status'], settings.STATUS_400)
        self.assertEqual(res['password'], None)
        assert not test_utils.select_user(self._phone_number)

    def test_user_creation(self):

        # first register the user
        expiration_time = int(time.time()) + int(config.get('registration', 'expiry_period_sec'))
        query = " INSERT INTO registered_users (authorization_code, expiration_time, phone_number) VALUES ( %s, %s, %s); "
        variables = (self._auth_code, expiration_time, self._phone_number)
        QueryHandler.execute(query, variables)

        # valid url params
        response = requests.get(self._creation_url + extra_params)
        res = json.loads(response.text)
        record = test_utils.select_user(phone_number = self._phone_number)

        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['password'], record[0]['password'])
        self.assertEqual(res['username'], record[0]['username'])
        old_username = record[0]['username']

        query = " SELECT * FROM registered_users WHERE phone_number = %s; "
        variables = (self._phone_number,)
        record = QueryHandler.get_results(query, variables)
        self.assertEqual(len(record), 0)

        expiration_time = int(time.time()) + int(config.get('registration', 'expiry_period_sec'))
        query = " INSERT INTO registered_users (authorization_code, expiration_time, phone_number) VALUES ( %s, %s, %s); "
        variables = (self._auth_code, expiration_time, self._phone_number)
        QueryHandler.execute(query, variables)

        response = requests.get(self._creation_url)
        res = json.loads(response.text)
        self.assertEqual(res['username'], old_username)

        query = " SELECT * FROM registered_users WHERE phone_number = %s; "
        variables = (self._phone_number,)
        record = QueryHandler.get_results(query, variables)
        assert not record

        query = " INSERT INTO registered_users (authorization_code, expiration_time, phone_number) VALUES ( %s, %s, %s); "
        variables = (self._auth_code, expiration_time, self._phone_number)
        QueryHandler.execute(query, variables)

        # invalid url params
        faulty_creation_url = tornado_local_address + "/create?phone_number=" + str(self._phone_number) \
                              + "&auth_code=" + str(self._auth_code)
        response = requests.get(faulty_creation_url)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_400)


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

#             test_utils.delete_user(username = self.username)

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
#         return api_v0_archive.Application()


# class ProfilePicServiceTest(AsyncHTTPTestCase):
#     _username = 'test'
#     _password = 'password'
#     _phone_number = config.get('tests', 'test_phone_number')

#     def setUp(self):
#         super(ProfilePicServiceTest, self).setUp()
#         try:
#             test_utils.create_user(username = self._username, password = self._password, phone_number = self._phone_number)

#             query = "INSERT INTO users (fb_id) VALUES (%s) WHERE username = %s;"
#             variables = (self._username, config.get('tests', 'test_facebook_id'))
#             QueryHandler.execute(query, variables)
#         except psycopg2.IntegrityError:
#             pass

#     def get_app(self):
#         return api_v0_archive.Application()

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
#         pass


class InterestTest(unittest.TestCase):
    _username = 'test'
    _password = 'password'
    _phone_number = config.get('tests', 'test_phone_number')
    _interests = [{"name": "interest_one", 'id': " test_1"}, 
    			{"name": "interest_two", 'id': "test_2"}, 
    			{"name": "interest_three", 'id': "test_3"}]
    _payload = {'username': _username, 'password': _password}
    _test_storage_url = tornado_local_address + "/set_user_interests"
    def get_app(self):
        return api_v0_archive.Application()

    def setUp(self):
        test_utils.delete_user(username = self._username, phone_number=self._phone_number)
        test_utils.create_user(username = self._username, password = self._password, phone_number = self._phone_number)
        

        query = " DELETE FROM users_interest WHERE username = %s;"
        variables = (self._username,)
        QueryHandler.execute(query, variables)

        query = " DELETE FROM interest WHERE "\
        +  " OR ".join([" interest_id = %s "] * len(self._interests)) + ";"
        variables = map(lambda interest: interest['id'], self._interests)
        QueryHandler.execute(query, variables)

        query = "INSERT INTO interest (interest_id, interest_name) VALUES " + ",".join(["(%s, %s)"] * len(self._interests))
        QueryHandler.execute(query, 
    						[self._interests[0]['id'], self._interests[0]['name'], 
    						self._interests[1]['id'], self._interests[1]['name'],
    						self._interests[2]['id'], self._interests[2]['name']]
        					)

    def test_storage(self):
    	payload = copy.copy(self._payload)
    	payload.update({'interests': map(lambda interest: interest['id'], self._interests[:2])})
    	payload.update(extra_params_dict)
        response = requests.post(self._test_storage_url, data = payload)
        res = json.loads(response.text)
        assert response

        self.assertEqual(res['status'], settings.STATUS_200)

        query = "select users.username, array_agg(interest.interest_name) as interests from users "\
            + " left outer join users_interest on (users.username = users_interest.username) "\
            + " left outer join interest on (users_interest.interest_id = interest.interest_id)"\
            + " WHERE users.username = %s group by users.username;"
        variables = (self._username,)
        record = QueryHandler.get_results(query, variables)
        assert record
        assert record[0]['username']
        assert record[0]['interests'] == map(lambda interest: interest['name'], self._interests[:2])

    	payload = copy.copy(self._payload)
    	payload.update({'interests': map(lambda interest: interest['id'], self._interests[2:])})
    	payload.update(extra_params_dict)

        response = requests.post(self._test_storage_url, data = payload)
        res = json.loads(response.text)
        assert response
        self.assertEqual(res['status'], settings.STATUS_200)

        query = "select users.username, array_agg(interest.interest_name) as interests from users "\
            + " left outer join users_interest on (users.username = users_interest.username) "\
            + " left outer join interest on (users_interest.interest_id = interest.interest_id)"\
            + " WHERE users.username = %s group by users.username;"
        variables = (self._username,)
        record = QueryHandler.get_results(query, variables)

        assert record
        assert record[0]['username']
        assert record[0]['interests'] == map(lambda interest: interest['name'], self._interests)

    def test_delete_interest(self):
    	payload = copy.copy(self._payload)
    	payload.update({'interests': map(lambda interest: interest['id'], self._interests)})
    	payload.update(extra_params_dict)
        response = requests.post(self._test_storage_url, data = payload)

    	payload = copy.copy(self._payload)
    	payload.update({'interests': map(lambda interest: interest['id'], self._interests[2:])})
    	payload.update(extra_params_dict)
        response = requests.delete(self._test_storage_url, data = payload)
        res = json.loads(response.text)
        assert response
        assert res['status'] == settings.STATUS_200

        query = "select users.username, array_agg(interest.interest_name) as interests from users "\
            + " left outer join users_interest on (users.username = users_interest.username) "\
            + " left outer join interest on (users_interest.interest_id = interest.interest_id)"\
            + " WHERE users.username = %s group by users.username;"
        variables = (self._username,)
        record = QueryHandler.get_results(query, variables)

        assert record
        assert record[0]['username']
        assert record[0]['interests'] == map(lambda interest: interest['name'], self._interests[:2])

    def tearDown(self):
        query = " DELETE FROM interest WHERE "\
            +  " OR ".join([" interest_id = %s "] * len(self._interests)) + ";"
        variables = map(lambda interest: interest['id'], self._interests)
        QueryHandler.execute(query, variables)
        test_utils.delete_user(username = self._username, phone_number=self._phone_number)


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
        self.url = tornado_local_address + "/media" + '?apk_version=v0.1&udid=TEST@UDID'
        self.media_presence_url = tornado_local_address + "/media_present?name=md5_sample" + extra_params
        file_name = sys.argv[0]
        file_content = open(file_name, 'r').read()
        md5 = hashlib.md5(file_content).hexdigest()
        headers = {'Checksum': 'md5_sample'}
        response = requests.post(self.url, headers=headers, data=file_content)
        self.assertEqual(json.loads(response.content)['status'], settings.STATUS_200)
        assert os.path.isfile('media/md5_sample')

        self.url = tornado_local_address + "/media?name=md5_sample" + extra_params
        response = requests.get(self.url)
        assert response.content

        response = requests.get(self.media_presence_url)
        self.assertEqual(json.loads(response.content)["status"], settings.STATUS_200)

        file_storage_name = "media/md5_sample"
        os.remove(file_storage_name)

        response = requests.get(self.media_presence_url)
        self.assertEqual(json.loads(response.content)["status"], settings.STATUS_400)

        self.url = tornado_local_address + "/media" + '?apk_version=v0.1&udid=TEST@UDID'
        file_name2 = 'big.mp4'
        headers = {'Checksum': 'big.mp4'}
        with open(file_name2, 'rb') as file_content2:
            response = requests.post(self.url, headers=headers, data=file_content2)
        self.assertEqual(json.loads(response.content)['status'], settings.STATUS_200)
        assert os.path.isfile('media/big.mp4')

        self.url = tornado_local_address + "/media?name=big.mp4" + extra_params
        response = requests.get(self.url)
        self.assertEqual(response.status_code, settings.STATUS_200)

    def get_app(self):
        return api_v0_archive.Application()

    def tearDown(self):
        pass


class IOSMediaHandlerTests(unittest.TestCase):
    url = None
    filename = None

    def setUp(self):
        self.url = tornado_local_address + '/media_multipart' + '?apk_version=v0.1&udid=TEST@UDID'
        self.test_files = [sys.argv[0]]

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
        self.assertEqual(response.status_code, settings.STATUS_200)
        self.assertEqual(res['status'], settings.STATUS_400)

        # 'Checksum' not provided
        response = requests.post(self.url, data=encoder.to_string(), headers={'Content-Type': encoder.content_type})
        res = json.loads(response.content)
        self.assertEqual(response.status_code, settings.STATUS_200)
        self.assertEqual(res['status'], settings.STATUS_400)

        # Request body not provided
        response = requests.post(self.url, headers={'Checksum': 'test_image', 'Content-Type': encoder.content_type})
        res = json.loads(response.content)
        self.assertEqual(response.status_code, settings.STATUS_200)
        self.assertEqual(res['status'], settings.STATUS_400)

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
            self.assertEqual(response.status_code, settings.STATUS_200)
            self.assertEqual(res['info'], 'Success')
            self.assertEqual(res['status'], settings.STATUS_200)


        # if file already exists
        self.filename = self.test_files[0]
        mime = magic.Magic(mime=True)
        mime_type = mime.from_file(self.filename)
        encoder = MultipartEncoder(
            fields={'name': 'image', 'filename': self.filename, 'Content-Disposition': 'form-data',
                    'Content-Type': mime_type, 'file': (self.filename, open(self.filename, 'rb'), mime_type)}
        )
        response = requests.post(self.url, data=encoder.to_string(),
                                 headers={'Content-Type': encoder.content_type, 'Checksum': file})
        res = json.loads(response.content)
        self.assertEqual(response.status_code, settings.STATUS_200)
        self.assertEqual(res['status'], settings.STATUS_422)

        # delete all test files from media folder
        for file in self.test_files:
            os.remove('media/' + file)





class ContactListTest(unittest.TestCase):
    _username = 'test'
    _password = 'password'
    _phone_number = config.get('tests', 'test_phone_number')
    _default_payload = {'apk_version': 'v1.0', 'udid' : '00'}
    _payload_auth = {'username': _username, 'password': _password}
    _friend_username = 'friend'
    _friend_password = 'test'
    _friend_phone_number = '91919191'
    _url = tornado_local_address + "/get_contact_jids"
    _contact_list_payload = {'contacts': [_friend_phone_number, '123']}

    def setUp(self):
        test_utils.delete_user(phone_number = self._phone_number)
        test_utils.create_user(phone_number = self._phone_number, username = self._username, password = self._password)

        test_utils.delete_user(phone_number = self._friend_phone_number)
        test_utils.create_user(phone_number = self._friend_phone_number, username = self._friend_username, password = self._friend_password)

    def test_unauthenticated_contacts_retrieval(self):
        fraud_auth_payload = {'username': 'test', 'password': 'asfdas'}
        payload = merge_dicts([self._default_payload, self._contact_list_payload, fraud_auth_payload])
        response = requests.post(self._url, data=payload)
        content = json.loads(response.content)
        assert content['status'] == settings.STATUS_404
        assert content['info'] == settings.BAD_AUTHENTICATION_ERROR
        assert not content.has_key('jids')

    def test_contacts_retrieval(self):
        payload = merge_dicts([self._default_payload, self._contact_list_payload, self._payload_auth])
        response = requests.post(self._url, data=payload)
        content = json.loads(response.content)
        assert content['status'] == settings.STATUS_200
        assert type(content['jids']) == list
        assert content['jids'][0]['username'] == self._friend_username
        assert len(content['jids']) == 1

    def tearDown(self):
        test_utils.delete_user(phone_number = self._phone_number)
        test_utils.delete_user(phone_number = self._friend_phone_number)


class NearbyUsersWithSameInterestsTests(unittest.TestCase):
    url = None
    username = None
    user_privacy_list = None
    users = []
    all_nearby_users = []
    interests = []
    interest_id = []
    users_interests = {}
    apk_version = 'v0.1'
    udid = 'abc'

    def create_test_users(self):
        for user in self.users:
            query = "INSERT INTO users(username, password, phone_number, lat, lng, last_seen, is_available, show_location) values(%s, %s, %s, %s, %s, %s, True, True);"
            variables = (user[0], user[1], user[2], user[3], user[4], user[5])
            try:
                QueryHandler.execute(query, variables)
            except psycopg2.IntegrityError, e:
                pass

    def create_interests(self):
        for interest in self.interests:
            query = "INSERT INTO interest(interest_id, interest_name) values(%s, %s) RETURNING interest_id;"
            variables = (interest['id'],interest['name'],)
            result = QueryHandler.get_results(query, variables)
            self.interest_id.append(result[0]['interest_id'])
        self.users_interests = {'test_1': [self.interest_id[0], self.interest_id[2]],
                                'test_2': [self.interest_id[1]],
                                'test_3': [self.interest_id[2], self.interest_id[3]],
                                'test_4': [self.interest_id[1], self.interest_id[0]],
                                'test_5': [self.interest_id[0], self.interest_id[1], self.interest_id[2], self.interest_id[3]],
                                'test_6': [self.interest_id[2], self.interest_id[1]]}

    def add_user_interests(self):
        try:
            for key,value in self.users_interests.items():
                for interest in value:
                    query = "INSERT INTO users_interest(interest_id, username) VALUES(%s, %s);"
                    variables = (interest, key)
                    QueryHandler.execute(query, variables)
        except psycopg2.IntegrityError, e:
            pass

    def add_roster_entry(self, friend, subscription = 'B'):

        query = "INSERT INTO rosterusers(username, jid, nick, subscription, ask, askmessage, server) VALUES" \
                "(%s, %s, %s, %s, %s, %s, %s);"
        variables = (self.username, friend, 't5', subscription, '', 'N', 'N')
        try:
            QueryHandler.execute(query, variables)
        except psycopg2.IntegrityError as e:
            pass

    def update_roster_entry(self, friend, subscription):
        query = "UPDATE rosterusers SET subscription = %s " \
                "WHERE username = %s AND jid = %s;"
        variables = (subscription, self.username, friend)
        QueryHandler.execute(query, variables)

    def add_user_privacy_list(self):
        test_utils.delete_user_from_table('username', 'privacy_list', self.username)

        query = "INSERT INTO privacy_list(username, name) values(%s, %s) RETURNING id;"
        variables = (self.username, self.user_privacy_list)
        result = QueryHandler.get_results(query, variables)
        self.list_id = result[0]['id']

    def set_blocked_contacts_for_user(self):
        self.blocked_users = ['test_1']
        for blocked in self.blocked_users:
            query = "INSERT INTO privacy_list_data(id, t, value, action, ord, match_all, match_iq, match_message, match_presence_in, match_presence_out)" \
                    "VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s);"
            variables = (self.list_id, 'j', blocked + '@mm.io', 'd', 1, 't', 't', 't', 't', 't')
            try:
                QueryHandler.execute(query, variables)
            except psycopg2.IntegrityError, e:
                pass

    def setUp(self):
        self.url = 'http://localhost:3000/get_nearby_users'
        self.username = 'test_4'
        self.password = 'pswd_4'
        self.user_privacy_list = 'test_4_privacy_list'
        self.epoch_time = int(time.time())
        self.users = [('test_1', 'pswd_1', 'test_1', 0.0000080, 0.0000080, self.epoch_time - 5370),
                      ('test_2', 'pswd_2', 'test_2', 0.0000090, 0.0000090, self.epoch_time - 1370),
                      ('test_3', 'pswd_3', 'test_3', 28.5232818, 77.1907957, self.epoch_time),
                      ('test_4', 'pswd_4', 'test_4', 0.0, 0.0, self.epoch_time),
                      ('test_5', 'pswd_5', 'test_5', 0.0000010, 0.0000010, self.epoch_time + 75),
                      ('test_6', 'pswd_6', 'test_6', 0.0, 0.0, self.epoch_time + 1000)]
        self.interests = [{'name':'test_interest_1', 'id': 'test_1'},
                        {'name':'test_interest_2', 'id': 'test_2'},
                        {'name':'test_interest_3', 'id': 'test_3'},
                        {'name':'test_interest_4', 'id': 'test_4'}]
        self.interest_id = []
        self.delete_users()
        self.delete_interests()
        self.create_test_users()
        self.create_interests()
        self.add_user_interests()
        self.add_roster_entry('test_5@mm.io')
        self.add_user_privacy_list()
        self.set_blocked_contacts_for_user()

    def delete_users(self):
        for user in self.users:
            test_utils.delete_user(username = user[0])

    def delete_interests(self):
        for interest in self.interests:
            query = "DELETE FROM interest where interest_id=%s;"
            variables = (interest['id'],)
            QueryHandler.execute(query, variables)

    def delete_user_friends(self):
        query = "DELETE FROM rosterusers WHERE username=%s;"
        variables = (self.username,)
        QueryHandler.execute(query, variables)

    def delete_user_privacy_list(self):
        query = "DELETE FROM privacy_list WHERE username=%s;"
        variables = (self.username,)
        QueryHandler.execute(query, variables)

    def modify_response(self, response):
        new_dict = {"friends" : {}, "anonymous": {}}
        for x in json.loads(response.content)["users"]:
            new_dict[x["friendship_status"]][x["username"]] = x["interests"]
        return new_dict

    def assert_response_status(self, response, expected_info, expected_status, expected_result=None):
        res = json.loads(response.text)
        self.assertEqual(res['info'], expected_info)
        self.assertEqual(res['status'], expected_status)
        modified_user_interest_dict = self.modify_response(response)
        if expected_result:
            assert modified_user_interest_dict == expected_result


    def test_get_nearby_users(self):
        # case 2: when 'test_2' was online 2 hours back
        query = "UPDATE users SET last_seen=%s WHERE username='test_2';"
        QueryHandler.execute(query, (time.time() - 7200,))
        self.expected_result_dict = {"friends": {"test_5": ["test_interest_2", "test_interest_1"]},
                                     "anonymous": {"test_6": ["test_interest_2"]}}
        self.data = {'username': 'test_4', 'password': 'pswd_4', 'lat': '0.0', 'lng': '0.0', 'radius': 5, 'apk_version': self.apk_version, 'udid': self.udid}
        response = requests.get(self.url, data=self.data)
        user_dict = {}
        self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)

        # case 1: when 'test_2' was online 15 minutes back
        query = "UPDATE users SET last_seen=%s WHERE username='test_2';"
        QueryHandler.execute(query, (time.time() - 900,))
        self.expected_result_dict = {"friends": {"test_5": ["test_interest_2", "test_interest_1"]},
                                     "anonymous": {"test_6": ["test_interest_2"], "test_2": ["test_interest_2"]}}
        self.data = {'username': 'test_4', 'password': 'pswd_4', 'lat': '0.0', 'lng': '0.0', 'radius': 5, 'apk_version': self.apk_version, 'udid': self.udid}
        response = requests.get(self.url, data=self.data)
        self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)


        # case 3: when 'test_6' is also a friend
        self.add_roster_entry('test_6@mm.io')
        self.expected_result_dict = {"friends": {"test_6": ["test_interest_2"],
                                                 "test_5": ["test_interest_2", "test_interest_1"]}, 
                                    "anonymous": {"test_2": ["test_interest_2"]}}
        self.data = {'username': 'test_4', 'password': 'pswd_4', 'lat': '0.0', 'lng': '0.0', 'radius': 5, 'apk_version': self.apk_version, 'udid': self.udid}
        response = requests.get(self.url, data=self.data)
        self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)

        # case 4: when 'test_2' has roster entry for none (not a friend)
        self.add_roster_entry('test_2@mm.io', subscription = 'N')
        response = requests.get(self.url, data=self.data)
        self.expected_result_dict = {"friends": {"test_6": ["test_interest_2"],
                                                 "test_5": ["test_interest_2", "test_interest_1"]}, 
                                    "anonymous": {"test_2": ["test_interest_2"]}}
        self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)

        #case 5: when 'test_2' has a both way subscription 
        self.update_roster_entry('test_2@mm.io', subscription = 'B')
        response = requests.get(self.url, data=self.data)
        self.expected_result_dict = {"friends": {"test_6": ["test_interest_2"],
                                                 "test_5": ["test_interest_2", "test_interest_1"],
                                                 "test_2": ["test_interest_2"]}, 
                                    "anonymous": {}}
        self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)

        # case 6: when 'test_2' has disabled his location
        self.expected_result_dict = {"friends": {"test_6": ["test_interest_2"],
                                                 "test_5": ["test_interest_2", "test_interest_1"]}, 
                                    "anonymous": {}}
        query = " UPDATE users SET show_location = FALSE WHERE username = %s;"
        variables = ('test_2',)
        QueryHandler.execute(query, variables)
        response = requests.get(self.url, data=self.data)
        self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)

        #case 7: when 'test_2' has enabled his location
        self.expected_result_dict = {"friends": {"test_6": ["test_interest_2"],
                                                 "test_5": ["test_interest_2", "test_interest_1"],
                                                 "test_2": ["test_interest_2"]}, 
                                    "anonymous": {}}
        query = " UPDATE users SET show_location = TRUE WHERE username = %s;"
        variables = ('test_2',)
        QueryHandler.execute(query, variables)
        response = requests.get(self.url, data=self.data)
        self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)

        #case 8: when 'test_2' has a 'F' subscription
        self.update_roster_entry('test_2@mm.io', subscription = 'F')
        response = requests.get(self.url, data=self.data)
        self.expected_result_dict = {"friends": {"test_6": ["test_interest_2"],
                                                 "test_5": ["test_interest_2", "test_interest_1"]}, 
                                    "anonymous": {"test_2": ["test_interest_2"]}}
        self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)

        # case 9: when 'test_2' has a 'T' subscription
        self.update_roster_entry('test_2@mm.io', subscription = 'T')
        response = requests.get(self.url, data=self.data)
        self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)
        

        # case 10: when user has NO friends
        self.delete_user_friends()
        self.expected_result_dict = {"friends": {}, 
                                    "anonymous": {"test_6": ["test_interest_2"], 
                                                "test_5": ["test_interest_2", "test_interest_1"],
                                                "test_2": ["test_interest_2"]}}
        self.data = {'username': 'test_4', 'password': 'pswd_4', 'lat': '0.0', 'lng': '0.0', 'radius': 5, 'apk_version': self.apk_version, 'udid': self.udid}
        response = requests.get(self.url, data=self.data)
        self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)


        # case 11: when no banned users
        query = "UPDATE users SET last_seen=%s WHERE username='test_1';"
        QueryHandler.execute(query, (time.time(),))
        self.delete_user_privacy_list()
        self.expected_result_dict = {"friends": {}, 
                                    "anonymous": {
                                        "test_6": ["test_interest_2"], 
                                        "test_5": ["test_interest_2", "test_interest_1"], 
                                        "test_1": ["test_interest_1"],
                                        "test_2": ["test_interest_2"]}}
        self.data = {'username': 'test_4', 'password': 'pswd_4', 'lat': '0.0', 'lng': '0.0', 'radius': 5, 'apk_version': self.apk_version, 'udid': self.udid}
        response = requests.get(self.url, data=self.data)
        self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)


    def test_no_nearby_users(self):
        self.data = {'username': 'test_4', 'password': 'pswd_4', 'lat': '23', 'lng': '23', 'radius': 5, 'apk_version': self.apk_version, 'udid': self.udid}
        response = requests.get(self.url, data=self.data)
        assert not json.loads(response.content)["users"]

    def test_no_interests(self):
        self.expected_result_dict = {"friends": {"test_5": []},
                                     "anonymous": {"test_6": [], "test_2": []}}
        self.delete_interests()
        self.data = {'username': 'test_4', 'password': 'pswd_4', 'lat': '0', 'lng': '0', 'radius': 5, 'apk_version': self.apk_version, 'udid': self.udid}
        response = requests.get(self.url, data=self.data)
        self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)

    def tearDown(self):
        self.delete_users()
        self.delete_interests()
        self.delete_user_friends()
        self.delete_user_privacy_list()


# class SendAppInvitationTest(unittest.TestCase):
#     _url = None
#     _unregistered_user = '910000000000'
#     _registered_user = '911111111111'
#     _invited_user = '912222222222'
#     _password = 'password'

#     def setUp(self):
#         self._url = tornado_local_address + '/send_app_invite' + '?apk_version=v0.1&udid=TEST@UDID'
#         test_utils.delete_user(phone_number=self._unregistered_user)
#         test_utils.delete_user(phone_number=self._registered_user)
#         test_utils.delete_user(phone_number=self._invited_user)
#         test_utils.create_user(username=self._registered_user, password=self._password, phone_number=self._registered_user)

#     def test_validation(self):

#         # incomplete post data
#         response = requests.post(self._url, data={})
#         res = json.loads(response.text)
#         self.assertEqual(res['info'], "Missing argument user")
#         self.assertEqual(res['status'], settings.STATUS_400)

#         # user not registered
#         response = requests.post(self._url, data={'user': self._unregistered_user, 'invited_user': self._invited_user})
#         res = json.loads(response.text)
#         self.assertEqual(res['info'], "Bad Request: User is not Registered!")
#         self.assertEqual(res['status'], settings.STATUS_400)

#         # invited user already registered
#         response = requests.post(self._url, data={'user': self._registered_user, 'invited_user': self._registered_user})
#         res = json.loads(response.text)
#         self.assertEqual(res['info'], "Bad Request: Invited User is Already Registered!")
#         self.assertEqual(res['status'], settings.STATUS_400)

#         # valid post request
#         response = requests.post(self._url, data={'user': self._registered_user, 'invited_user': self._invited_user})
#         res = json.loads(response.text)
#         self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
#         self.assertEqual(res['status'], settings.STATUS_200)

class MatchPushNotificationTest(unittest.TestCase):
    _register_url = tornado_local_address + "/user_register_match"
    _unregister_url = tornado_local_address + "/user_unregister_match"
    _username = "test"
    _phone_number = "911"
    _password = "test"
    _match = "test_match"
    _match_id = "000"
    def delete_users_match(self):
        query = "DELETE FROM users_matches WHERE users_matches.username = %s AND users_matches.match_id = %s;"
        variables = (self._username, self._match_id,)
        QueryHandler.execute(query, variables)
        
    def insert_match(self):
        query = "INSERT INTO matches (id, name) VALUES (%s, %s);"
        variables = (self._match_id, self._match)
        QueryHandler.execute(query, variables)

    def delete_match(self):
        query = " DELETE FROM matches WHERE id = %s;"
        variables = (self._match_id,)
        QueryHandler.execute(query, variables)

    def setUp(self):
        test_utils.delete_user(username = self._username)
        test_utils.create_user(username = self._username, password = self._password, phone_number = self._phone_number)
        self.delete_users_match()
        self.delete_match()
        self.insert_match()

    def test_register_and_unregister_match(self):
        payload = {"username": self._username, "password": self._password, "match_id": self._match_id}
        payload.update(extra_params_dict)

        response = json.loads(requests.post(self._register_url, data=payload).content)
        assert response['status'] == settings.STATUS_200

        query = " SELECT * FROM users_matches WHERE users_matches.username = %s AND users_matches.match_id = %s;"
        variables = (self._username, self._match_id,)
        record = QueryHandler.get_results(query, variables)[0]
        assert record
        assert record['username'] == self._username
        assert record['match_id'] == self._match_id

        response = json.loads(requests.post(self._unregister_url, data=payload).content)
        assert response['status'] == settings.STATUS_200

        query = " SELECT * FROM users_matches WHERE users_matches.username = %s AND users_matches.match_id = %s;"
        variables = (self._username, self._match_id,)
        record = QueryHandler.get_results(query, variables)
        assert not record
    
    def tearDown(self):
        test_utils.delete_user(username = self._username)
        self.delete_users_match()
        self.delete_match()



class SetDeviceTokenReturnUserMatchesTest(object):
    _username = "test"
    _phone_number = "911"
    _password = "test"
    _token = "test_token"
    _match = "test_match"
    _match_id = "000"

    def delete_users_match(self):
        query = "DELETE FROM users_matches WHERE users_matches.username = %s AND users_matches.match_id = %s;"
        variables = (self._username, self._match_id,)
        QueryHandler.execute(query, variables)
        
    def insert_match(self):
        query = "INSERT INTO matches (id, name) VALUES (%s, %s);"
        variables = (self._match_id, self._match)
        QueryHandler.execute(query, variables)

    def delete_match(self):
        query = " DELETE FROM matches WHERE id = %s;"
        variables = (self._match_id,)
        QueryHandler.execute(query, variables)

    def set_user_match(self):
        query = " INSERT INTO users_matches (username, match_id) VALUES (%s, %s);"
        variables = (self._username, self._match_id)
        QueryHandler.execute(query, variables)

    def setUp(self):
        test_utils.delete_user(username = self._username)
        test_utils.create_user(username = self._username, password = self._password, phone_number = self._phone_number)
        self.delete_users_match()
        self.delete_match()
        self.insert_match()
        self.set_user_match()

    def test_set_unset_token_and_return_user_matches(self):
        payload = {"username": self._username, "password": self._password, "token": self._token}
        payload.update(extra_params_dict)
        response = json.loads(requests.post(self._set_url, data=payload).content)
        assert response['status'] == settings.STATUS_200

        record = test_utils.select_user(username = self._username)[0]
        assert record['username'] == self._username
        assert record[self._token_name] == self._token
        assert len(response["match_ids"]) == 1
        assert response["match_ids"][0] == self._match_id
        assert record['device_id'] == extra_params_dict['udid']

        if self._unset_url:
            payload = {'username': self._username, 'password': self._password}
            payload.update(extra_params_dict)
            response = json.loads(requests.post(self._unset_url, data=payload).content)
            assert response['status'] == settings.STATUS_200

            record = test_utils.select_user(username = self._username)[0]
            assert record['username'] == self._username
            assert not record[self._token_name]
            assert record['device_id']        

    def tearDown(self):
        test_utils.delete_user(username = self._username)
        self.delete_users_match()
        self.delete_match()

class SetAndroidDeviceTokenReturnUserMatchesTest(SetDeviceTokenReturnUserMatchesTest, unittest.TestCase, ):
    _set_url = tornado_local_address + "/set_android_token_and_return_user_matches"
    _unset_url = tornado_local_address + "/remove_android_token"
    _token_name = "android_token"
    

class IOSSetUserDeviceIdReturnUserMatchesTests(SetDeviceTokenReturnUserMatchesTest, unittest.TestCase):
    _set_url = tornado_local_address + "/set_ios_token_and_return_user_matches"
    _unset_url = None
    _token_name = "apple_token"

class SetLocationPrivacyTest(unittest.TestCase):
    _set_location_privacy_url = tornado_local_address + "/set_location_privacy"
    _username = "test"
    _phone_number = "911"
    _password = "test"
    _token = "test_token"
    
    def setUp(self):
        test_utils.delete_user(username = self._username)
        test_utils.create_user(username = self._username, password = self._password, phone_number = self._phone_number)

    def test_show_location_status(self):
        payload = {"username": self._username, "password": self._password, "show_location_status": "true"}
        payload.update(extra_params_dict)
        response = json.loads(requests.post(self._set_location_privacy_url, data=payload).content)
        assert response['status'] == settings.STATUS_200

        record = test_utils.select_user(username = self._username)[0]
        assert record['username'] == self._username
        assert record['show_location']

        payload = {'username': self._username, 'password': self._password, "show_location_status": "false"}
        payload.update(extra_params_dict)
        response = json.loads(requests.post(self._set_location_privacy_url, data=payload).content)
        assert response['status'] == settings.STATUS_200

        record = test_utils.select_user(username = self._username)[0]
        assert record['username'] == self._username
        assert not record['show_location']

    def tearDown(self):
        test_utils.delete_user(username = self._username)

class PushNotifcationsTest(unittest.TestCase):
    #TO-DO Write test for users retrieval 
    _push_notification_url = tornado_local_address + "/notify_event"
    _sport_code = "1"
    _event_code = "1"
    _match_id = "1"
    _league_id = "1"

    def test_notify_event(self):
        payload = {"s": self._sport_code, "e": self._event_code, "m": self._match_id, "tt": "test", "bt": "test", "l": self._league_id}
        response = json.loads(requests.post(self._push_notification_url, data=payload).content)
        assert response['status'] == settings.STATUS_200

class ApnsHandlerTest(unittest.TestCase):
	def test_singleton(self):
		from notification_handler import ApnsHandler
		class_1 = ApnsHandler()
		class_2 = ApnsHandler()
		assert id(class_1) == id(class_2)

class RegisterMatch(unittest.TestCase):
    _url = tornado_local_address + "/register_matches"
    _matches_1 = [{"name": "test_1", "id": "id_1"}, {"name": "test_2", "id": "id_2"}]
    _matches_2 = [{"name": "test_1", "id": "id_1"}, {"name": "test_3", "id": "id_3"}]

    def test_match_registration(self):
        payload = {"matches": self._matches_1}
        response = json.loads(requests.post(self._url, data=json.dumps(payload)).content)
        assert response["status"] == 200

        match_1_ids = set(map(lambda x: x["id"], self._matches_1))
        query = "SELECT id FROM matches;"
        results = QueryHandler.get_results(query, ())
        assert match_1_ids.issubset(set(map(lambda x: x["id"], results)))

        payload = {"matches": self._matches_2}
        response = json.loads(requests.post(self._url, data=json.dumps(payload)).content)
        assert response["status"] == 200

        match_1_2_ids = set(map(lambda x: x["id"], self._matches_1 + self._matches_2))
        query = "SELECT id FROM matches;"
        results = QueryHandler.get_results(query, ())
        assert match_1_2_ids.issubset(set(map(lambda x: x["id"], results)))

    def tearDown(self):
        query = " DELETE FROM matches WHERE id IN (%s);"
        ids = ",".join(list(set(map(lambda x: x['id'], self._matches_1 + self._matches_2))))  
        QueryHandler.execute(query, (ids,))

if __name__ == '__main__':
    unittest.main()
