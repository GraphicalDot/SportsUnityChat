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

class UsersWatchingMatchTest(unittest.TestCase):
    _username = "test"
    _phone_number = "911"
    _password = "test"
    _test_storage_url = tornado_local_address + "/v1/set_user_watching_match"
    _test_deletion_url = tornado_local_address + "/v1/delete_user_watching_match"
    _test_retrieval_url = tornado_local_address + "/v1/friends_watching"

    _first_friends_username = "test1"
    _first_friends_password = "password"
    _first_friends_phone_number = "912"


    _second_friends_username = "test2"
    _second_friends_password = "password"
    _second_friends_phone_number = "913"

    _match_id_1 = "test|1"
    _match_id_2 = "test|2"


    def setUp(self):
        test_utils.delete_user(username = self._username)
        test_utils.create_user(username = self._username, password = self._password, phone_number = self._phone_number)

        test_utils.delete_user(username = self._first_friends_username)
        test_utils.create_user(username = self._first_friends_username, password = self._first_friends_password, phone_number = self._first_friends_phone_number)

        test_utils.delete_user(username = self._second_friends_username)
        test_utils.create_user(username = self._second_friends_username, password = self._second_friends_password, phone_number = self._second_friends_phone_number)

        self.add_friend(self._first_friends_username, 'B')
        self.add_friend(self._second_friends_username, 'B')

        self.unregister_matches(self._first_friends_username)
        self.unregister_matches(self._second_friends_username)

        self.register_match(self._first_friends_username, self._match_id_1)
        self.register_match(self._second_friends_username, self._match_id_1)


    def test_storage(self):
        payload = {"username": self._username, "password": self._password, "match_id": self._match_id_1}
        payload.update(extra_params_dict)
        response = requests.post(self._test_storage_url, data = json.dumps(payload))
        res = json.loads(response.text)
        assert response
        self.assertEqual(res['status'], settings.STATUS_200)


        query = "SELECT * FROM users_watching_matches"\
            + " WHERE username = %s;"
        variables = (self._username,)
        record = QueryHandler.get_results(query, variables)
        assert record
        assert record[0]['username'] == self._username
        assert record[0]['match_id'] == self._match_id_1


        response = requests.post(self._test_deletion_url, data = json.dumps(payload))
        res = json.loads(response.text)
        assert response
        self.assertEqual(res['status'], settings.STATUS_200)


        query = "SELECT * FROM users_watching_matches"\
            + " WHERE username = %s;"
        variables = (self._username,)
        record = QueryHandler.get_results(query, variables)
        assert not record


    def update_roster_entry(self, friend, subscription):
        query = "UPDATE rosterusers SET subscription = %s " \
                "WHERE username = %s AND jid = %s;"
        variables = (subscription, self._username, friend)
        QueryHandler.execute(query, variables)

    def add_friend(self, friend, subscription = 'B'):
        query = "INSERT INTO rosterusers(username, jid, nick, subscription, ask, askmessage, server) VALUES" \
                "(%s, %s, %s, %s, %s, %s, %s);"
        variables = (self._username, friend, 't5', subscription, '', 'N', 'N')
        try:
            QueryHandler.execute(query, variables)
        except psycopg2.IntegrityError as e:
            pass

    def register_match(self, username, match_id):
        query = " INSERT INTO users_watching_matches (username, match_id) VALUES (%s, %s);"
        variables = (username, match_id,)
        QueryHandler.execute(query, variables)

    def unregister_match(self, username, match_id):
        query = " DELETE FROM users_watching_matches WHERE username = %s AND match_id = %s;"
        variables = (username, match_id,)
        QueryHandler.execute(query, variables)

    def unregister_matches(self, username):
        query = " DELETE FROM users_watching_matches WHERE username = %s;"
        variables = (username,)
        QueryHandler.execute(query, variables)      

    def delete_user_friends(self, username):
        query = "DELETE FROM rosterusers WHERE username=%s;"
        variables = (username,)
        QueryHandler.execute(query, variables)


    def test_retrieve_friends_watching_the_match(self):
        # from IPython import embed
        # embed()
        payload = {"username": self._username, "password": self._password, "matches": [self._match_id_1, self._match_id_2]}
        payload.update(extra_params_dict)
        response = json.loads(requests.post(self._test_retrieval_url, data=payload).content)
        assert response['status'] == settings.STATUS_200
        assert response['matches'][self._match_id_1]
        assert not response['matches'].get(self._match_id_2)

        assert type(response['matches'][self._match_id_1]) == list
        assert len(response['matches'][self._match_id_1]) == 2
        assert self._first_friends_username in response['matches'][self._match_id_1]
        assert self._second_friends_username in response['matches'][self._match_id_1]

        # Case: When one of the friends has unregistered the match

        self.unregister_match(self._second_friends_username, self._match_id_1)
        response = json.loads(requests.post(self._test_retrieval_url, data=payload).content)
        assert self._first_friends_username in response['matches'][self._match_id_1]
        assert not self._second_friends_username in response['matches'][self._match_id_1]

        # Case: When one of the friends has subscribed to the second match
        self.register_match(self._second_friends_username, self._match_id_2)
        response = json.loads(requests.post(self._test_retrieval_url, data=payload).content)
        assert self._first_friends_username in response['matches'][self._match_id_1]
        assert self._second_friends_username in response['matches'][self._match_id_2]

        # Case: When first user  is not friends
        self.update_roster_entry(self._first_friends_username, 'N')
        response = json.loads(requests.post(self._test_retrieval_url, data=payload).content)
        assert not response['matches'].get(self._match_id_1)
        assert self._second_friends_username in response['matches'][self._match_id_2]

    def test_empty_match_list(self):
        payload = {"username": self._username, "password": self._password, "matches": []}
        payload.update(extra_params_dict)
        response = json.loads(requests.post(self._test_retrieval_url, data=payload).content)
        assert response['status'] == settings.STATUS_200
        assert not response.get('matches')

    def tearDown(self):
        test_utils.delete_user(username = self._username)
        test_utils.delete_user(username = self._first_friends_username)
        test_utils.delete_user(username = self._second_friends_username)
        self.unregister_matches(self._first_friends_username)
        self.unregister_matches(self._second_friends_username)
        self.delete_user_friends(self._username)


if __name__ == '__main__':
    unittest.main()