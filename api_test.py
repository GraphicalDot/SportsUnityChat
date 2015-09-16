from tornado.testing import AsyncHTTPTestCase, LogTrapTestCase
import unittest
from global_func import QueryHandler, S3Handler
import api_v0_archive
import facebook
from IPython import embed 
import json
import os, sys
import requests
import psycopg2
import psycopg2.extras
import xmltodict, json
from xml.etree import cElementTree as ET
from ConfigParser import ConfigParser
from sport_notifications import NotificationAdapter
from football_notifications import FootballNotifications
config = ConfigParser()
config.read('config.py')


class SportNotificationTest(unittest.TestCase):
	_sports_notifications = [FootballNotifications]
	_test_football_data =  {
		'league_id' : "league_id",
		'home_team'  : "home_team_name",
		'away_team' : "away_team_name",
		'match_date' : "date",
		'match_id' : "match_id",
		'home_team_score' : "home_team_score",
		'away_team_score' : "away_team_score",
		'match_status' : "match_status",
		'match_time' : "match_time"
	}

	_xml_football_data_element = [
		"league_id" ,
		"home_team" ,
		"away_team" ,
		"match_id" ,
		"home_team_score" ,
		"away_team_score" ,
		"match_status" ,
		"match_time"
	] 


	def test_stanza_creation(self):
		for sport in _sports_notifications:
			notification = NotificationAdapter(_test_football_data, sport)
			xml = ET.tostring(notification.get_xml_stanza())
			xml_dict = xmltodict.parse(xml)
			for xml_element in _xml_football_data_element:
				assert xml_dict[xml_element]



class UserTest(unittest.TestCase):
	
	def test_user_authentication(self):
		from api_v0_archive import User
		username = "test"
		password = "password"

		query = " INSERT INTO users (username, password) VALUES (%s,%s);"
		variables = (username, password,)

		record = QueryHandler.execute(query, variables)

		user = User(username, password)

		user_exists = user.authenticate()

		fraud_password = 'test'

		fraud_user = User(username, fraud_password)
		user_not_exists = fraud_user.authenticate()

		query = " DELETE FROM users WHERE username = %s;"
		variables = (username, )
		QueryHandler.execute(query, variables)

		assert user_exists
		assert not user_not_exists

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
		response = self.wait(timeout = 20)
		print response.body
		username = self._phone_number + config.get('xmpp','domain')
		query = " SELECT * FROM registered_users WHERE username = %s "
		variables = (username,)
		record = QueryHandler.get_results(query, variables)
		assert record
		self.assertEqual(200, json.loads(response.body)['status'])

	def test_wrong_auth_code_failure(self):
		username = self._phone_number + config.get('xmpp','domain')
		query = " UPDATE registered_users SET authorization_code = '12345'"\
				" WHERE username = %s; "
		variables = (username,)
		QueryHandler.execute(query, variables)

		self.http_client.fetch(self.get_url(self._creation_url), self.stop)
		response = self.wait(timeout = 20)

		query = " SELECT * FROM users WHERE username = %s; "
		variables = (self._phone_number,)
		record = QueryHandler.get_results(query, variables)

		self.assertNotEqual(json.loads(response.body)['status'], 200)
		self.assertEqual(json.loads(response.body)['password'], None)

	def test_user_creation(self):
		query = " DELETE FROM users WHERE username = %s;"
		variables = (self._phone_number,)
		QueryHandler.execute(query, variables)

		username = self._phone_number + config.get('xmpp','domain')
		query = " UPDATE registered_users SET authorization_code = %s"\
				" WHERE username = %s; "
		variables = (self._auth_code, username,)
		QueryHandler.execute(query, variables)

		self.http_client.fetch(self.get_url(self._creation_url), self.stop)
		response = self.wait(timeout = 20)

		query = " SELECT * FROM users WHERE username = %s; "
		variables = (self._phone_number,)
		record = QueryHandler.get_results(query, variables)

		self.assertEqual(str(username), record[0]['username']+config.get('xmpp','domain'))
		self.assertEqual(json.loads(response.body)['status'], 200)
		self.assertEqual(json.loads(response.body)['password'], record[0]['password'])

		query = " SELECT * FROM registered_users WHERE username = %s; "
		variables = (username,)
		record = QueryHandler.get_results(query, variables)
		self.assertEqual(len(record), 0)

class FacebookFriendServiceTest(AsyncHTTPTestCase):

	_facebook_id = 145634995501895
	_id = '9560488236@mm.io'
	_token = config.get('database','facebook_token') 
	_get_facebook_friends = '/fb_friends?fb_id=' + str(_facebook_id) + '&token=' + str(_token) + '&id=9560488236@mm.io'

	def get_app(self):
		return api_v0_archive.make_app()

	def test_fb_graph_api(self):
		self.http_client.fetch(self.get_url(self._get_facebook_friends), self.stop)
		response = self.wait(timeout = 20)
		self.assertEqual(200, json.loads(response.body)['status'])

	def test_fb_id_storage(self):
		self.http_client.fetch(self.get_url(self._get_facebook_friends), self.stop)
		response = self.wait(timeout = 20)
		query = " SELECT * FROM users WHERE fb_id = %s ;"
		results = QueryHandler.get_results(query, (self._facebook_id, ))
		self.assertEqual(results[0]['username'], str.split(self._id,'@')[0])

class PubSubServiceTest(AsyncHTTPTestCase):

	_publish_score = '/publish_score?sport=football&score'
	_new_event = '/new_event?name=BorussiaDortmund'
	_cricket_commentary = '/cricket_notifications'
	_tennis_notifications = '/tennis_notifications'
	_football_notifications = '/football_notifications'
	
	def get_app(self):
		return api_v0_archive.make_app()
	
	def test_xml_creator(self):
		from pubsub import PubSubNotificationClient
		node = "example"
		message = "example"
		pubsub = PubSubNotificationClient(node, message)
		xml = ET.tostring(pubsub.create_xml_stanza())
		xml_dict = xmltodict.parse(xml)
		assert xml_dict['pubsub']
		assert xml_dict['pubsub']['publish']
		assert xml_dict['pubsub']['publish']['item']
		assert xml_dict['pubsub']['publish']['item']
		assert xml_dict['pubsub']['publish']['item']['entry']
		assert xml_dict['pubsub']['publish']['item']['room_id']
		assert xml_dict['pubsub']['publish']['item']['update']

	def test_publish_score(self):
		from pubsub import PubSubEventClient
		pass

	def test_cricket_notifications(self):
		# payload_one = {'ball': '1', 'run':'121', 'wickets':'2', 'commentary':'wonderful match', 'number_of_overs':'1', 'batting':'IND', 'bowling': 'AUS'}
		# headers = {'content-type': 'application/json'}
		# response_one = requests.post(url, data=json.dumps(payload_one), headers=headers)
		# self.assertEqual(200, json.loads(response.body)['status'])
		raise NotImplementedError

	def test_tennis_notifications(self):
		payload_one = {
			'data' : {
				"date": "2015-09-1",
				"final_score": "1  :  3",
				"match_staus": "Finished",
				"players": "Coric B. vs Nadal R.",
				"sets": [
				  "3 - 6",
				  "2 - 6",
				  "6 - 4",
				  "4 - 6"
				],
				"tournament": "ATP Singles: US Open"
			}
		}
		headers = {'content-type': 'application/json'}
		response_one = requests.post(self._tennis_notifications, data=json.dumps(payload_one), headers=headers)
		self.assertEqual(200, json.loads(response.body)['status'])

	def test_football_notifications(self):
		payload_one = {
			'league_id' : "league_id",
			'home_team'  : "home_team_name",
			'away_team' : "away_team_name",
			'match_id' : "match id",
			'home_team_score' : "home team score",
			'away_team_score' : "away team score",
			'match_status' : "match status",
			'match_time' : "match tim"
		}
		headers = {'content-type': 'application/json'}
		response_one = requests.post(self._football_notifications, data=json.dumps(payload_one), headers=headers)
		self.assertEqual(200, json.loads(response.body)['status'])


	def test_tennis_pubsub_node_creator(self):
		self.http_client.fetch(self.get_url(self._new_event), self.stop)
		response = self.wait(timeout = 20)
		self.assertEqual(json.loads(response.body)['status'], 200)		
		assert json.loads(response.body)['name']		


class ProfilePicServiceTest(AsyncHTTPTestCase):

	_profile_pic = '/profile_pic'
	
	def setUp(self):
		try:
			self.username = 'test'
			self.password = 'password'
			query = "INSERT INTO users (username, password) VALUES (%s, %s);"
			variables = (self.username, self.password)
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
		response = requests.post('http://localhost:3000/profile_pic', data=data, files=file_data)
		self.assertEqual(json.loads(response.text)['status'], 200)

		file_data = {'file': open(file_name, 'rb')}
		data = {
			'username': self.username,
			'password': 'password1'
		}		
		response = requests.post('http://localhost:3000/profile_pic', data=data, files=file_data)		
		self.assertNotEqual(json.loads(response.text)['status'], 200)

		profile_pic_bucket = config.get('amazon', 'profile_pics_bucket')
		s3 = S3Handler(profile_pic_bucket)

		file_name = self.username
		s3_file_url = "https://%s.s3.amazonaws.com/%s" % (profile_pic_bucket, file_name) 
		s3_file_response = requests.get(s3_file_url)

		self.assertEqual(200, s3_file_response.status_code)


	def tearDown(self):
		query = "DELETE FROM users WHERE username = %s;"
		variables = (self.username,)
		QueryHandler.execute(query, variables)



if __name__ == '__main__':
	unittest.main()