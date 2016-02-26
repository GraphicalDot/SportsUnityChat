import json
import requests
import settings
import test_utils
import time
import unittest
from global_func import QueryHandler

LOCAL_ADDRESS = 'http://{}:{}'.format(settings.TORNADO_LOCALHOST, settings.TORANDO_PORT)


class AdminSelectUsersTest(unittest.TestCase):
    url = None

    def setUp(self):
        self.url = LOCAL_ADDRESS + '/get_users'

    def test_get(self):
        response = requests.get(self.url)
        self.assertEqual(response.status_code, settings.STATUS_200)


class AdminCreateUserTest(unittest.TestCase):
    url = None
    username = '910000000000'
    password = 'password'

    def setUp(self):
        self.url = LOCAL_ADDRESS + '/create_user'
        test_utils.delete_user(username=self.username)

    def test_get(self):
        response = requests.get(self.url)
        self.assertEqual(response.status_code, settings.STATUS_200)

    def test_post(self):

        # empty post data
        response = requests.post(self.url, data={})
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_400)
        self.assertEqual(res['info'], "Missing argument username")

        # incomplete post data
        response = requests.post(self.url, data={'username': self.username})
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_400)
        self.assertEqual(res['info'], "Missing argument password")

        # valid post data
        response = requests.post(self.url, data={'username': self.username, 'password': self.password, 'phone_number': self.username})
        res = json.loads(response.text)
        query = "SELECT * FROM users WHERE phone_number=%s;"
        variables = (self.username,)
        result = QueryHandler.get_results(query, variables)
        self.assertEqual(len(result), 1)
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(result[0]['lat'], 0.0)
        self.assertEqual(result[0]['lng'], 0.0)
        self.assertEqual(result[0]['fb_id'], None)
        self.assertEqual(result[0]['fb_name'], "")
        self.assertEqual(result[0]['apple_udid'], "")
        self.assertEqual(result[0]['password'], self.password)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        self.assertEqual(res['status'], settings.STATUS_200)


class AdminUpdateUserTest(unittest.TestCase):
    url = None
    username = '910000000000'

    def setUp(self):
        self.url = LOCAL_ADDRESS + '/update_user'

    def test_get(self):
        response = requests.get(self.url)
        self.assertEqual(response.status_code, settings.STATUS_200)

    def test_post(self):

        # incomplete post data
        response = requests.post(self.url, data={})
        res = json.loads(response.text)
        self.assertEqual(res['info'], "Missing argument phone_number")
        self.assertEqual(res['status'], settings.STATUS_400)

        # complete data with no updations
        query = "SELECT * FROM users WHERE username=%s;"
        variables = (self.username,)
        before_result = QueryHandler.get_results(query, variables)

        response = requests.post(self.url, data={'phone_number': self.username})
        res = json.loads(response.text)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        self.assertEqual(res['status'], settings.STATUS_200)

        after_result = QueryHandler.get_results(query, variables)
        self.assertEqual(before_result[0]['lat'], after_result[0]['lat'])
        self.assertEqual(after_result[0]['is_available'], False)

        # complete data with few updates
        response = requests.post(self.url, data={'phone_number': self.username, 'is_available': True, 'lat': 0.0})
        res = json.loads(response.text)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        self.assertEqual(res['status'], settings.STATUS_200)

        after_result = QueryHandler.get_results(query, variables)
        self.assertEqual(after_result[0]['lat'], 0.0)
        self.assertEqual(after_result[0]['is_available'], True)


class AdminDeleteUserTest(unittest.TestCase):
    url = None
    phone_number = username = '912222222222'
    password = 'password'

    def setUp(self):
        self.url = LOCAL_ADDRESS + '/delete_user'
        test_utils.delete_user(username=self.username, phone_number=self.phone_number)
        test_utils.create_user(username=self.username, password=self.password, phone_number=self.phone_number)

    def test_get(self):
        response = requests.get(self.url)
        self.assertEqual(response.status_code, settings.STATUS_200)

    def test_post(self):

        # incomplete post data
        response = requests.post(self.url, data={})
        res = json.loads(response.text)
        self.assertEqual(res['info'], "Missing argument phone_number")
        self.assertEqual(res['status'], settings.STATUS_400)

        # complete data
        response = requests.post(self.url, data={'phone_number': self.username})
        res = json.loads(response.text)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        self.assertEqual(res['status'], settings.STATUS_200)
        time.sleep(10)

        query = "SELECT * FROM users WHERE phone_number=%s;"
        variables = (self.phone_number,)
        result = QueryHandler.get_results(query, variables)
        self.assertEqual(len(result), 0)


if __name__ == '__main__':
    unittest.main()
