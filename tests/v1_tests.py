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
from common.funcs import QueryHandler, S3, merge_dicts
from requests_toolbelt.multipart.encoder import MultipartEncoder
from requests_toolbelt import MultipartEncoder
from shutil import copyfile
from common.custom_error import BadAuthentication
from models.user import User
import settings
import test_utils
import copy
from wand.color import Color
from wand.image import Image as wImage
from models.s3_image import S3Image
import base64

config = ConfigParser()
config.read(os.path.dirname(__file__) + '/../config.py')

extra_params = '&apk_version=v0.1&udid=TEST@UDID'
extra_params_dict = {'apk_version' : 'v0.1', 'udid' : "test_udid"}
tornado_listening_port =  int(config.get('tornado', 'listening_port'))
tornado_local_address =  "http://localhost:%u" % tornado_listening_port

class SetLocationPrivacyTest(unittest.TestCase):
    _set_location_privacy_url = tornado_local_address + "/v1/set_location_privacy"
    _username = "test"
    _phone_number = "911"
    _password = "test"
    _token = "test_token"
    
    def setUp(self):
        test_utils.delete_user(username = self._username)
        test_utils.create_user(username = self._username, password = self._password, phone_number = self._phone_number)


    def test_show_location_status(self):
        payload = {"username": self._username, "password": self._password, "show_location_status": "a"}
        payload.update(extra_params_dict)
        response = json.loads(requests.post(self._set_location_privacy_url, data=payload).content)
        assert response['status'] == settings.STATUS_200

        record = test_utils.select_user(username = self._username)[0]
        assert record['username'] == self._username
        assert record['show_location'] == 'a'

        payload = {"username": self._username, "password": self._password, "show_location_status": "f"}
        payload.update(extra_params_dict)
        response = json.loads(requests.post(self._set_location_privacy_url, data=payload).content)
        assert response['status'] == settings.STATUS_200

        record = test_utils.select_user(username = self._username)[0]
        assert record['username'] == self._username
        assert record['show_location'] == 'f'

        payload = {'username': self._username, 'password': self._password, "show_location_status": "n"}
        payload.update(extra_params_dict)
        response = json.loads(requests.post(self._set_location_privacy_url, data=payload).content)
        assert response['status'] == settings.STATUS_200

        record = test_utils.select_user(username = self._username)[0]
        assert record['username'] == self._username
        assert record['show_location'] == 'n'

    def tearDown(self):
        test_utils.delete_user(username = self._username)

class InterestTest(unittest.TestCase):
    _username = 'test'
    _password = 'password'
    _phone_number = config.get('tests', 'test_phone_number')
    _interests = [{"name": "interest_one", 'id': " test_1", "properties": 
        {
            "obj_name": "Afghanistan",
            "sports_type": "cricket",
            "filter_type": "teams",
            "obj_id": "212",
            "obj_flag": "http:\/\/players.images.s3.amazonaws.com\/212.png"
        }
    }, 
    {"name": "interest_two", 'id': "test_2", "properties": 
        {
            "obj_name": "Afghanistan",
            "sports_type": "cricket",
            "filter_type": "teams",
            "obj_id": "212",
            "obj_flag": "http:\/\/players.images.s3.amazonaws.com\/212.png"
        }
    }, 
    {"name": "interest_three", 'id': "test_3", "properties": 
        {
            "obj_name": "Afghanistan",
            "sports_type": "cricket",
            "filter_type": "teams",
            "obj_id": "212",
            "obj_flag": "http:\/\/players.images.s3.amazonaws.com\/212.png"
        }
    }]
    _payload = {'username': _username, 'password': _password}
    _test_storage_url = tornado_local_address + "/v1/set_user_interests"

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
        payload.update({'interests': map(lambda interest:{'id': interest['id'], 'properties': interest['properties']}, self._interests[:2])})
        payload.update(extra_params_dict)
        response = requests.post(self._test_storage_url, data = json.dumps(payload))
        res = json.loads(response.text)
        assert response


        self.assertEqual(res['status'], settings.STATUS_200)

        query = "select users.username, json_agg(row_to_json((SELECT d FROM (SELECT interest.interest_name AS name, users_interest.properties) d))) AS  interests from users "\
            + " left outer join users_interest on (users.username = users_interest.username) "\
            + " left outer join interest on (users_interest.interest_id = interest.interest_id)"\
            + " WHERE users.username = %s group by users.username;"
        variables = (self._username,)
        record = QueryHandler.get_results(query, variables)
        assert record
        assert record[0]['username']
        assert record[0]['interests'] == map(lambda interest: {'name': interest['name'], 'properties': interest['properties']}, self._interests[:2])

        payload = copy.copy(self._payload)
        payload.update({'interests': map(lambda interest: {'id': interest['id'], 'properties': interest['properties']}, self._interests[2:])})
        payload.update(extra_params_dict)

        response = requests.post(self._test_storage_url, data = json.dumps(payload))
        res = json.loads(response.text)
        assert response
        self.assertEqual(res['status'], settings.STATUS_200)

        query = "select users.username, json_agg(row_to_json((SELECT d FROM (SELECT interest.interest_name AS name, users_interest.properties) d))) AS  interests from users "\
            + " left outer join users_interest on (users.username = users_interest.username) "\
            + " left outer join interest on (users_interest.interest_id = interest.interest_id)"\
            + " WHERE users.username = %s group by users.username;"
        variables = (self._username,)
        record = QueryHandler.get_results(query, variables)

        assert record
        assert record[0]['username']
        assert record[0]['interests'] == map(lambda interest: {'name': interest['name'], 'properties': interest['properties']}, self._interests[2:])
        

    def test_delete_interest(self):
        payload = copy.copy(self._payload)
        payload.update({'interests': map(lambda interest: {'id': interest['id'], 'properties': interest['properties']}, self._interests)})
        payload.update(extra_params_dict)
        response = requests.post(self._test_storage_url, data = json.dumps(payload))

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

if __name__ == '__main__':
    unittest.main()