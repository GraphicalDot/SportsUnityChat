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

if __name__ == '__main__':
    unittest.main()