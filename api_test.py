from tornado.testing import AsyncHTTPTestCase, LogTrapTestCase
import unittest
from api_v0_archive import QueryHandler
import api_v0_archive
import facebook
from IPython import embed 
import json
import psycopg2
import psycopg2.extras

class FacebookFriendServiceTest(AsyncHTTPTestCase):
	_facebook_id = 145634995501895
	_id = '9560488236@mm.io'
	_token = 'CAACEdEose0cBAFgiXWoNZBeHLodUKVZBl2ykO4vB5QWZCw04b7MvnXyrKFdBXdZCACHZBBekjcZA'\
	'LYlv9vUZBnVzreYSiIeMTbOOIfLRBE2gjUHaMjO2qOow15fJ5Wp9XxyZCI5WCT5LoWGBP4U8G4yRHZCGscFi4H'\
	'HRfcjzwaUZBDUZChbGaZBkE72Y91Ara83ewrRJ3yr7Au12oAqLF2we2i6c'
	_get_facebook_friends = '/fb_friends?fb_id=' + str(_facebook_id) + '&token=' + str(_token) + '&id=9560488236@mm.io'

	def get_app(self):
		return api_v0_archive.make_app()


	def test_fb_graph_api(self):
		self.http_client.fetch(self.get_url(self._get_facebook_friends), self.stop)
		response = self.wait()
		print json.loads(response.body)['info']
		self.assertEqual(200, json.loads(response.body)['status'])

	def test_fb_id_storage(self):
		self.http_client.fetch(self.get_url(self._get_facebook_friends), self.stop)
		response = self.wait()
		query = " SELECT * FROM users WHERE fb_id = %s ;"
		results = QueryHandler.get_results(query, (self._facebook_id, ))
		self.assertEqual(results[0]['username'], str.split(self._id,'@')[0])


class RegistrationTest(AsyncHTTPTestCase):
	_phone_number = '9560488236'
	_auth_code = 'ASDFG'
	_registration_url = "/register?phone_number=" + str(_phone_number)
	_authorization_url = "/authorize?phone_number=" + str(_phone_number)\
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

	def test_user_authorization(self):
		username = self._phone_number + "@mm.io"
		query = " UPDATE registered_users SET authorization_code = %s"\
				" WHERE username = %s; "
		variables = (self._auth_code, username,)
		QueryHandler.execute(query, variables)

		self.http_client.fetch(self.get_url(self._authorization_url), self.stop)
		response = self.wait()

		query = " SELECT * FROM users WHERE username = %s; "
		variables = (self._phone_number,)
		record = QueryHandler.get_results(query, variables)

		self.assertEqual(str(self._phone_number), record[0]['username'])
		self.assertEqual(json.loads(response.body)['status'], 200)
		self.assertEqual(json.loads(response.body)['password'], record[0]['password'])

if __name__ == '__main__':
	unittest.main()