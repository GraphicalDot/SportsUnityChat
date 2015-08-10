from tornado.testing import AsyncHTTPTestCase, LogTrapTestCase
import unittest
from api_v0_archive import QueryHandler
import api_v0_archive
import facebook
from IPython import embed 
import json
import psycopg2
import psycopg2.extras



class RegistrationTest(AsyncHTTPTestCase):
	_phone_number = '9560488236'
	_auth_code = 'ASDFG'
	_registration_url = "/register?phone_number=" + str(_phone_number)
	_creation_url = "/create?phone_number=" + str(_phone_number)\
						+ "&auth_code=" + str(_auth_code)
	
	def get_app(self):
		return api_v0_archive.make_app()

	def test_user_registration(self):
		self.http_client.fetch(self.get_url(self._registration_url), self.stop)
		response = self.wait()
		print response.body
		username = self._phone_number + "@mm.io"
		query = " SELECT * FROM registered_users WHERE username = %s "
		variables = (username,)
		record = QueryHandler.get_results(query, variables)
		assert record
		self.assertEqual(200, json.loads(response.body)['status'])

	def test_wrong_auth_code_failure(self):
		username = self._phone_number + "@mm.io"
		query = " UPDATE registered_users SET authorization_code = '12345'"\
				" WHERE username = %s; "
		variables = (username,)
		QueryHandler.execute(query, variables)

		self.http_client.fetch(self.get_url(self._creation_url), self.stop)
		response = self.wait(timeout = 20)

		query = " SELECT * FROM users WHERE username = %s; "
		variables = (self._phone_number,)
		record = QueryHandler.get_results(query, variables)

		self.assertEqual(str(username), record[0]['username']+"@mm.io")
		self.assertNotEqual(json.loads(response.body)['status'], 200)
		self.assertEqual(json.loads(response.body)['password'], None)

	def test_user_creation(self):
		username = self._phone_number + "@mm.io"
		query = " UPDATE registered_users SET authorization_code = %s"\
				" WHERE username = %s; "
		variables = (self._auth_code, username,)
		QueryHandler.execute(query, variables)

		self.http_client.fetch(self.get_url(self._creation_url), self.stop)
		response = self.wait(timeout = 20)

		query = " SELECT * FROM users WHERE username = %s; "
		variables = (self._phone_number,)
		record = QueryHandler.get_results(query, variables)

		self.assertEqual(str(username), record[0]['username']+"@mm.io")
		self.assertEqual(json.loads(response.body)['status'], 200)
		self.assertEqual(json.loads(response.body)['password'], record[0]['password'])

class FacebookFriendServiceTest(AsyncHTTPTestCase):
	_facebook_id = 145634995501895
	_id = '9560488236@mm.io'
	_token = 'CAACEdEose0cBAIDsa7frvOzodUblHU2QI0RZBfOLJ83ZC1H5tjWv6fewitEiBX2IsmgdDF30p6eodF0pQ8FAkcA63OaipqIv608QZBsxUPmJWoY9kb8pEOFJCBWyY6LPZA80RlQl3sk0K2KZCZCrgb2KBMviXadyu5llk4Eii2ZCLL9AeX3LkhIfEBZAeG3xEVkN5HIR4RfsQFFpeLP3uthB'
	_get_facebook_friends = '/fb_friends?fb_id=' + str(_facebook_id) + '&token=' + str(_token) + '&id=9560488236@mm.io'

	def get_app(self):
		return api_v0_archive.make_app()




	def test_fb_graph_api(self):
		self.http_client.fetch(self.get_url(self._get_facebook_friends), self.stop)
		response = self.wait()
		self.assertEqual(200, json.loads(response.body)['status'])

	def test_fb_id_storage(self):
		self.http_client.fetch(self.get_url(self._get_facebook_friends), self.stop)
		response = self.wait()
		query = " SELECT * FROM users WHERE fb_id = %s ;"
		results = QueryHandler.get_results(query, (self._facebook_id, ))
		self.assertEqual(results[0]['username'], str.split(self._id,'@')[0])

if __name__ == '__main__':
	unittest.main()