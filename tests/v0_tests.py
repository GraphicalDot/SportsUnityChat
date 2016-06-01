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
from models.interest import Interest
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

class CreationTest(unittest.TestCase):
	username = None
	_phone_number = config.get('tests', 'test_phone_number')
	_password = 'password'
	_username = 'test'
	_auth_code = 'ASDFG'
	_name = 'test'
	_registration_url = tornado_local_address + "/register?phone_number=" + str(_phone_number)
	_creation_url = tornado_local_address + "/create?phone_number=" + str(_phone_number) \
					+ "&auth_code=" + str(_auth_code) + extra_params

	_set_user_info_url = tornado_local_address + "/set_user_info"
	_interests = [{"name": "interest_one", 'id': " test_1"}, 
				{"name": "interest_two", 'id': "test_2"}, 
				{"name": "interest_three", 'id': "test_3"}]
	_set_interest_url = tornado_local_address + "/set_user_interests"

	def setUp(self):
		super(CreationTest, self).setUp()
		test_utils.delete_user(username=self._username, phone_number=self._phone_number)
		test_utils.delete_registered_user(phone_number=self._phone_number)
		for interest in self._interests:
			Interest(interest['name'], interest['id']).delete()
			Interest(interest['name'], interest['id']).create()

	def test_user_registration(self):

		# test for banned user with valid url
		query = " INSERT INTO users (username, password, phone_number, is_banned) VALUES (%s,%s, %s, True);"
		variables = (self._username, self._password, self._phone_number,)
		QueryHandler.execute(query, variables)

		response = requests.get(self._registration_url + extra_params)
		res = json.loads(response.text)

		self.assertEqual(res['status'], settings.STATUS_403)
		self.assertEqual(res['info'], settings.USER_FORBIDDEN_ERROR)


		# test for non-banned user with valid url
		query = "UPDATE users SET is_banned=False WHERE phone_number=%s;"
		variables = (self._phone_number,)
		QueryHandler.execute(query, variables)
		response = requests.get(self._registration_url + extra_params)
		res = json.loads(response.text)
		self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
		self.assertEqual(res['status'], settings.STATUS_200)

	def test_user_registration_function(self):
		user = User(phone_number = self._phone_number)
		response, status = user._register()
		assert status == settings.STATUS_200

		select_query = "SELECT * FROM registered_users WHERE phone_number = %s;"
		select_variables = (self._phone_number,)
		record = QueryHandler.get_results(select_query, select_variables)
		assert len(record) == 1
		assert record[0]['gateway_response']
		assert record[0]['phone_number']

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


		query = " SELECT DISTINCT users.name, users.username AS username, password, show_location, array_agg(users_interest.interest_id) AS interests "\
		+	" FROM users LEFT OUTER JOIN users_interest on (users.username = users_interest.username) WHERE phone_number = %s  GROUP BY users.username;"
		variables = (self._phone_number, )
		record = QueryHandler.get_results(query, variables)

		self.assertEqual(res['status'], settings.STATUS_200)
		self.assertEqual(res['password'], record[0]['password'])
		self.assertEqual(res['username'], record[0]['username'])
		old_username = record[0]['username']
		old_password = record[0]['password']
		assert not record[0]['show_location'] 

		query = " SELECT * FROM registered_users WHERE phone_number = %s; "
		variables = (self._phone_number,)
		record = QueryHandler.get_results(query, variables)
		self.assertEqual(len(record), 0)

		# set user info
		payload = {'username': old_username, 'password': old_password, 'interests': [self._interests[0]['id'], self._interests[1]['id']]}
		payload.update(extra_params_dict)
		response = json.loads(requests.post(self._set_interest_url, data = payload).content)
		assert response['status'] == settings.STATUS_200

		payload = {'username': old_username, 'password': old_password, 'name': self._name}
		payload.update(extra_params_dict)
		response = json.loads(requests.post(self._set_user_info_url, data = payload).content)
		assert response['status'] == settings.STATUS_200


		expiration_time = int(time.time()) + int(config.get('registration', 'expiry_period_sec'))
		query = " INSERT INTO registered_users (authorization_code, expiration_time, phone_number) VALUES ( %s, %s, %s); "
		variables = (self._auth_code, expiration_time, self._phone_number)
		QueryHandler.execute(query, variables)

		response = requests.get(self._creation_url)
		res = json.loads(response.text)
		self.assertEqual(res['username'], old_username)
		assert res['name'] == self._name
		assert len(res['interests']) == 2
		assert self._interests[0]['id'] in res['interests'] 
		assert self._interests[1]['id'] in res['interests'] 

		
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


	def tearDown(self):
		test_utils.delete_user(phone_number=self._phone_number)
		for interest in self._interests:
			Interest(interest['id'], interest['name']).delete()


class ProfilePicServiceTest(unittest.TestCase):
	_username = 'test'
	_password = 'password'
	_phone_number = config.get('tests', 'test_phone_number')
	_test_set_url = tornado_local_address + "/set_dp"
	_test_get_url = tornado_local_address + "/get_dp"
	_small_version_name = str(_username) + "/S" + ".jpg"
	_large_version_name = str(_username) + "/L" + ".jpg"
	_profile_pic_bucket = config.get('amazon', 'dp_bucket_name')
	_groupname = 'test'

	def setUp(self):
		test_utils.delete_user(username = self._username, phone_number=self._phone_number)
		test_utils.create_user(username = self._username, password = self._password, phone_number = self._phone_number)
		S3(self._profile_pic_bucket).delete_key(self._small_version_name)
		S3(self._profile_pic_bucket).delete_key(self._large_version_name)

	def test_profile_pic_updation(self):
		image_data = base64.b64encode(wImage(width=640, height=640, background=Color('red')).make_blob(format='png'))

		payload = {
			'username': self._username,
			'password': self._password,
			'jid': self._groupname,
			'content': image_data
		}
		payload.update(extra_params_dict)
		response = requests.post(self._test_set_url , data = json.dumps(payload))
		assert json.loads(response.text)['status'] == 200

		time.sleep(10)

		assert S3(self._profile_pic_bucket).check_exists(self._large_version_name)
		assert S3(self._profile_pic_bucket).check_exists(self._small_version_name)

		payload = {
			'username': self._username,
			'password': self._password,
			'jid': self._groupname,
			'version': 'L'
		}

		payload.update(extra_params_dict)
		response = json.loads(requests.post(self._test_get_url , data = json.dumps(payload)).content)
		assert response['status'] == 200
		assert base64.b64decode(response['content']) == S3(self._profile_pic_bucket).get_file(self._large_version_name)

	def tearDown(self):
		pass

class InterestTest(unittest.TestCase):
	_username = 'test'
	_password = 'password'
	_phone_number = config.get('tests', 'test_phone_number')
	_interests = [{"name": "interest_one", 'id': " test_1"}, 
				{"name": "interest_two", 'id': "test_2"}, 
				{"name": "interest_three", 'id': "test_3"}]
	_payload = {'username': _username, 'password': _password}
	_test_storage_url = tornado_local_address + "/set_user_interests"

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
		assert record[0]['interests'] == map(lambda interest: interest['name'], self._interests[2:])
		

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


class MediaTest(unittest.TestCase):

	def setUp(self):
		super(MediaTest, self).setUp()
		file_storage_name = "media/md5_sample"
		if os.path.isfile(file_storage_name):
			os.remove(file_storage_name)

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

	def get_app(self):
		return api_v0_archive.Application()

	def tearDown(self):
		pass


class IOSMediaHandlerTests(unittest.TestCase):
	url = None
	filename = None

	def setUp(self):
		self.url = tornado_local_address + '/media_multipart' + '?apk_version=v0.1&udid=TEST@UDID'
		self.test_file = sys.argv[0]
		self.filename = "test_file"
		try:
			os.remove('media/' + self.filename)
		except Exception, e:
			pass

	def test_upload_media(self):

		# test on different media files
		mime = magic.Magic(mime=True)
		mime_type = mime.from_file(self.test_file)
		encoder = MultipartEncoder(
			fields={'name': 'image', 'filename': self.filename, 'Content-Disposition': 'form-data',
					'Content-Type': mime_type, 'file': (self.filename, open(self.test_file, 'rb'), mime_type)}
			)

		response = requests.post(self.url, data=encoder.to_string(),
								 headers={'Content-Type': encoder.content_type, 'Checksum': self.filename})
		res = json.loads(response.content)
		self.assertEqual(response.status_code, settings.STATUS_200)
		self.assertEqual(res['info'], 'Success')
		self.assertEqual(res['status'], settings.STATUS_200)


		# if file already exists
		mime = magic.Magic(mime=True)
		mime_type = mime.from_file(self.test_file)
		encoder = MultipartEncoder(
			fields={'name': 'image', 'filename': self.filename, 'Content-Disposition': 'form-data',
					'Content-Type': mime_type, 'file': (self.filename, open(self.test_file, 'rb'), mime_type)}
		)
		response = requests.post(self.url, data=encoder.to_string(),
								 headers={'Content-Type': encoder.content_type, 'Checksum': self.filename})
		res = json.loads(response.content)
		self.assertEqual(response.status_code, settings.STATUS_200)
		self.assertEqual(res['status'], settings.STATUS_422)

	def tearDown(self):
		os.remove('media/' + self.filename)


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
			query = "INSERT INTO users(username, password, phone_number, lat, lng, last_seen, is_available, show_location) values(%s, %s, %s, %s, %s, %s, True, %s);"
			variables = (user[0], user[1], user[2], user[3], user[4], user[5], settings.SHOW_LOCATION_ALL_STATUS)
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
			assert  set(expected_result["friends"].keys()).issubset(set(modified_user_interest_dict["friends"].keys()))
			assert  set(expected_result["anonymous"].keys()).issubset(set(modified_user_interest_dict["anonymous"].keys()))


	def test_get_nearby_users_when_show_location_is_all(self):
		# case : when 'test_2' was online 10 hours back
		query = "UPDATE users SET last_seen=%s WHERE username='test_2';"
		QueryHandler.execute(query, (time.time() - 36000,))
		self.expected_result_dict = {"friends": {"test_5": ["test_interest_2", "test_interest_1"]},
									 "anonymous": {"test_6": ["test_interest_2"]}}
		self.data = {'username': 'test_4', 'password': 'pswd_4', 'lat': '0.0', 'lng': '0.0', 'radius': 5, 'apk_version': self.apk_version, 'udid': self.udid}
		response = requests.get(self.url, data=self.data)
		user_dict = {}
		self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)

		# case : when 'test_2' was online 15 minutes back
		query = "UPDATE users SET last_seen=%s WHERE username='test_2';"
		QueryHandler.execute(query, (time.time() - 900,))
		self.expected_result_dict = {"friends": {"test_5": ["test_interest_2", "test_interest_1"]},
									 "anonymous": {"test_6": ["test_interest_2"], "test_2": ["test_interest_2"]}}
		self.data = {'username': 'test_4', 'password': 'pswd_4', 'lat': '0.0', 'lng': '0.0', 'radius': 5, 'apk_version': self.apk_version, 'udid': self.udid}
		response = requests.get(self.url, data=self.data)
		self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)


		# case : when 'test_6' is also a friend
		self.add_roster_entry('test_6@mm.io')
		self.expected_result_dict = {"friends": {"test_6": ["test_interest_2"],
												 "test_5": ["test_interest_2", "test_interest_1"]}, 
									"anonymous": {"test_2": ["test_interest_2"]}}
		self.data = {'username': 'test_4', 'password': 'pswd_4', 'lat': '0.0', 'lng': '0.0', 'radius': 5, 'apk_version': self.apk_version, 'udid': self.udid}
		response = requests.get(self.url, data=self.data)
		self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)

		# case : when 'test_2' has roster entry for none (not a friend)
		self.add_roster_entry('test_2@mm.io', subscription = 'N')
		response = requests.get(self.url, data=self.data)
		self.expected_result_dict = {"friends": {"test_6": ["test_interest_2"],
												 "test_5": ["test_interest_2", "test_interest_1"]}, 
									"anonymous": {"test_2": ["test_interest_2"]}}
		self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)

		#case : when 'test_2' has a both way subscription 
		self.update_roster_entry('test_2@mm.io', subscription = 'B')
		response = requests.get(self.url, data=self.data)
		self.expected_result_dict = {"friends": {"test_6": ["test_interest_2"],
												 "test_5": ["test_interest_2", "test_interest_1"],
												 "test_2": ["test_interest_2"]}, 
									"anonymous": {}}
		self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)

		# case : when 'test_2' has disabled his location
		self.expected_result_dict = {"friends": {"test_6": ["test_interest_2"],
												 "test_5": ["test_interest_2", "test_interest_1"]}, 
									"anonymous": {}}
		query = " UPDATE users SET show_location = %s WHERE username = %s;"
		variables = (settings.SHOW_LOCATION_NONE_STATUS ,'test_2',)
		QueryHandler.execute(query, variables)
		response = requests.get(self.url, data=self.data)
		self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)

		#case : when 'test_2' has set his location to all
		self.expected_result_dict = {"friends": {"test_6": ["test_interest_2"],
												 "test_5": ["test_interest_2", "test_interest_1"],
												 "test_2": ["test_interest_2"]}, 
									"anonymous": {}}
		query = " UPDATE users SET show_location = %s WHERE username = %s;"
		variables = (settings.SHOW_LOCATION_ALL_STATUS, 'test_2',)
		QueryHandler.execute(query, variables)
		response = requests.get(self.url, data=self.data)
		self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)

		#case 7: when 'test_2' has set his location to friends
		self.expected_result_dict = {"friends": {"test_6": ["test_interest_2"],
												 "test_5": ["test_interest_2", "test_interest_1"],
												 "test_2": ["test_interest_2"]}, 
									"anonymous": {}}
		query = " UPDATE users SET show_location = %s WHERE username = %s;"
		variables = (settings.SHOW_LOCATION_FRIENDS_STATUS, 'test_2',)
		QueryHandler.execute(query, variables)
		response = requests.get(self.url, data=self.data)
		self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)

		#case 8: when 'test_2' has a 'F' subscription and location to all
		self.update_roster_entry('test_2@mm.io', subscription = 'F')
		query = " UPDATE users SET show_location = %s WHERE username = %s;"
		variables = (settings.SHOW_LOCATION_ALL_STATUS, 'test_2',)
		QueryHandler.execute(query, variables)
		response = requests.get(self.url, data=self.data)
		self.expected_result_dict = {"friends": {"test_6": ["test_interest_2"],
												 "test_5": ["test_interest_2", "test_interest_1"]}, 
									"anonymous": {"test_2": ["test_interest_2"]}}
		self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)

		#case 8: when 'test_2' has a 'F' subscription and location to friends
		self.update_roster_entry('test_2@mm.io', subscription = 'F')
		query = " UPDATE users SET show_location = %s WHERE username = %s;"
		variables = (settings.SHOW_LOCATION_FRIENDS_STATUS, 'test_2',)
		QueryHandler.execute(query, variables)
		response = requests.get(self.url, data=self.data)
		self.expected_result_dict = {"friends": {"test_6": ["test_interest_2"],
												 "test_5": ["test_interest_2", "test_interest_1"]}, 
									"anonymous": {}}
		self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)

		# case 9: when 'test_2' has a 'T' subscription and location to all
		self.expected_result_dict = {"friends": {"test_6": ["test_interest_2"],
										 "test_5": ["test_interest_2", "test_interest_1"]}, 
							"anonymous": {"test_2": ["test_interest_2"]}}
		self.update_roster_entry('test_2@mm.io', subscription = 'T')
		query = " UPDATE users SET show_location = %s WHERE username = %s;"
		variables = (settings.SHOW_LOCATION_ALL_STATUS, 'test_2',)
		QueryHandler.execute(query, variables)
		response = requests.get(self.url, data=self.data)
		self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)
		
		# case 9: when 'test_2' has a 'T' subscription and location to friends
		self.expected_result_dict = {"friends": {"test_6": ["test_interest_2"],
										 "test_5": ["test_interest_2", "test_interest_1"]}, 
							"anonymous": {}}
		self.update_roster_entry('test_2@mm.io', subscription = 'T')
		query = " UPDATE users SET show_location = %s WHERE username = %s;"
		variables = (settings.SHOW_LOCATION_FRIENDS_STATUS, 'test_2',)
		QueryHandler.execute(query, variables)
		response = requests.get(self.url, data=self.data)
		self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)


		# case 10: when user has NO friends :( and test_2's location is visible to all
		self.delete_user_friends()
		query = " UPDATE users SET show_location = %s WHERE username = %s;"
		variables = (settings.SHOW_LOCATION_ALL_STATUS, 'test_2',)
		QueryHandler.execute(query, variables)
		self.expected_result_dict = {"friends": {}, 
									"anonymous": {"test_6": ["test_interest_2"], 
												"test_5": ["test_interest_2", "test_interest_1"],
												"test_2": ["test_interest_2"]}}
		self.data = {'username': 'test_4', 'password': 'pswd_4', 'lat': '0.0', 'lng': '0.0', 'radius': 5, 'apk_version': self.apk_version, 'udid': self.udid}
		response = requests.get(self.url, data=self.data)
		self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)


		# case 10: when user has NO friends :( and test_2's location is visible to friends
		self.delete_user_friends()
		query = " UPDATE users SET show_location = %s WHERE username = %s;"
		variables = (settings.SHOW_LOCATION_FRIENDS_STATUS, 'test_2',)
		QueryHandler.execute(query, variables)
		
		self.expected_result_dict = {"friends": {}, 
									"anonymous": {}}
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
										"test_1": ["test_interest_1"]}}
		self.data = {'username': 'test_4', 'password': 'pswd_4', 'lat': '0.0', 'lng': '0.0', 'radius': 5, 'apk_version': self.apk_version, 'udid': self.udid}
		response = requests.get(self.url, data=self.data)
		self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)

		#case 12: when the user has disabled his privacy
		query = "UPDATE users SET show_location = %s WHERE username = %s;"
		variables = (settings.SHOW_LOCATION_NONE_STATUS, self.username, )
		QueryHandler.execute(query, variables)
		self.expected_result_dict = {"friends": {}, 
									"anonymous": {}}
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

	def test_get_nearby_users_when_show_location_status_is_friends(self):
		query = "UPDATE users SET show_location=%s WHERE username='test_4';"
		variables = (settings.SHOW_LOCATION_FRIENDS_STATUS,)
		QueryHandler.execute(query, variables)
		
		# case : when 'test_2' was online 10 hours back
		query = "UPDATE users SET last_seen=%s WHERE username='test_2';"
		QueryHandler.execute(query, (time.time() - 36000,))
		self.expected_result_dict = {"friends": {"test_5": ["test_interest_2", "test_interest_1"]},
									 "anonymous": {}}
		self.data = {'username': 'test_4', 'password': 'pswd_4', 'lat': '0.0', 'lng': '0.0', 'radius': 5, 'apk_version': self.apk_version, 'udid': self.udid}
		response = requests.get(self.url, data=self.data)
		user_dict = {}
		self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)

		# case : when 'test_2' was online 15 minutes back
		query = "UPDATE users SET last_seen=%s WHERE username='test_2';"
		QueryHandler.execute(query, (time.time() - 900,))
		self.expected_result_dict = {"friends": {"test_5": ["test_interest_2", "test_interest_1"]},
									 "anonymous": {}}
		self.data = {'username': 'test_4', 'password': 'pswd_4', 'lat': '0.0', 'lng': '0.0', 'radius': 5, 'apk_version': self.apk_version, 'udid': self.udid}
		response = requests.get(self.url, data=self.data)
		self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)


		# case : when 'test_6' is also a friend
		self.add_roster_entry('test_6@mm.io')
		self.expected_result_dict = {"friends": {"test_6": ["test_interest_2"],
												 "test_5": ["test_interest_2", "test_interest_1"]}, 
									"anonymous": {}}
		self.data = {'username': 'test_4', 'password': 'pswd_4', 'lat': '0.0', 'lng': '0.0', 'radius': 5, 'apk_version': self.apk_version, 'udid': self.udid}
		response = requests.get(self.url, data=self.data)
		self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)

		# case : when 'test_2' has roster entry for none (not a friend)
		self.add_roster_entry('test_2@mm.io', subscription = 'N')
		response = requests.get(self.url, data=self.data)
		self.expected_result_dict = {"friends": {"test_6": ["test_interest_2"],
												 "test_5": ["test_interest_2", "test_interest_1"]}, 
									"anonymous": {}}
		self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)

		#case : when 'test_2' has a both way subscription 
		self.update_roster_entry('test_2@mm.io', subscription = 'B')
		response = requests.get(self.url, data=self.data)
		self.expected_result_dict = {"friends": {"test_6": ["test_interest_2"],
												 "test_5": ["test_interest_2", "test_interest_1"],
												 "test_2": ["test_interest_2"]}, 
									"anonymous": {}}
		self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)

		# case : when 'test_2' has disabled his location
		self.expected_result_dict = {"friends": {"test_6": ["test_interest_2"],
												 "test_5": ["test_interest_2", "test_interest_1"]}, 
									"anonymous": {}}
		query = " UPDATE users SET show_location = %s WHERE username = %s;"
		variables = (settings.SHOW_LOCATION_NONE_STATUS ,'test_2',)
		QueryHandler.execute(query, variables)
		response = requests.get(self.url, data=self.data)
		self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)

		#case : when 'test_2' has enabled his location
		self.expected_result_dict = {"friends": {"test_6": ["test_interest_2"],
												 "test_5": ["test_interest_2", "test_interest_1"],
												 "test_2": ["test_interest_2"]}, 
									"anonymous": {}}
		query = " UPDATE users SET show_location = %s WHERE username = %s;"
		variables = (settings.SHOW_LOCATION_ALL_STATUS, 'test_2',)
		QueryHandler.execute(query, variables)
		response = requests.get(self.url, data=self.data)
		self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)


		#case : when 'test_2' has a 'F' subscription and location to all
		self.update_roster_entry('test_2@mm.io', subscription = 'F')
		query = " UPDATE users SET show_location = %s WHERE username = %s;"
		variables = (settings.SHOW_LOCATION_ALL_STATUS, 'test_2',)
		QueryHandler.execute(query, variables)
		response = requests.get(self.url, data=self.data)
		self.expected_result_dict = {"friends": {"test_6": ["test_interest_2"],
												 "test_5": ["test_interest_2", "test_interest_1"]}, 
									"anonymous": {}}
		self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)

		#case : when 'test_2' has a 'F' subscription and location to friends
		self.update_roster_entry('test_2@mm.io', subscription = 'F')
		query = " UPDATE users SET show_location = %s WHERE username = %s;"
		variables = (settings.SHOW_LOCATION_FRIENDS_STATUS, 'test_2',)
		QueryHandler.execute(query, variables)
		response = requests.get(self.url, data=self.data)
		self.expected_result_dict = {"friends": {"test_6": ["test_interest_2"],
												 "test_5": ["test_interest_2", "test_interest_1"]}, 
									"anonymous": {}}
		self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)

		# case 9: when 'test_2' has a 'T' subscription and location to all
		self.expected_result_dict = {"friends": {"test_6": ["test_interest_2"],
										 "test_5": ["test_interest_2", "test_interest_1"]}, 
							"anonymous": {}}
		self.update_roster_entry('test_2@mm.io', subscription = 'T')
		query = " UPDATE users SET show_location = %s WHERE username = %s;"
		variables = (settings.SHOW_LOCATION_ALL_STATUS, 'test_2',)
		QueryHandler.execute(query, variables)
		response = requests.get(self.url, data=self.data)
		self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)
		
		# case 9: when 'test_2' has a 'T' subscription and location to friends
		self.expected_result_dict = {"friends": {"test_6": ["test_interest_2"],
										 "test_5": ["test_interest_2", "test_interest_1"]}, 
							"anonymous": {}}
		self.update_roster_entry('test_2@mm.io', subscription = 'T')
		query = " UPDATE users SET show_location = %s WHERE username = %s;"
		variables = (settings.SHOW_LOCATION_FRIENDS_STATUS, 'test_2',)
		QueryHandler.execute(query, variables)
		response = requests.get(self.url, data=self.data)
		self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)

		# case 10: when user has NO friends
		self.delete_user_friends()
		self.expected_result_dict = {"friends": {}, 
									"anonymous": {}}
		self.data = {'username': 'test_4', 'password': 'pswd_4', 'lat': '0.0', 'lng': '0.0', 'radius': 5, 'apk_version': self.apk_version, 'udid': self.udid}
		response = requests.get(self.url, data=self.data)
		self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)


		# case 11: when no banned users
		query = "UPDATE users SET last_seen=%s WHERE username='test_1';"
		QueryHandler.execute(query, (time.time(),))
		self.delete_user_privacy_list()
		self.expected_result_dict = {"friends": {}, 
									"anonymous": {}}
		self.data = {'username': 'test_4', 'password': 'pswd_4', 'lat': '0.0', 'lng': '0.0', 'radius': 5, 'apk_version': self.apk_version, 'udid': self.udid}
		response = requests.get(self.url, data=self.data)
		self.assert_response_status(response, settings.SUCCESS_RESPONSE, settings.STATUS_200, self.expected_result_dict)

		#case 12: when the user has disabled his privacy
		query = "UPDATE users SET show_location = %s WHERE username = %s;"
		variables = (settings.SHOW_LOCATION_NONE_STATUS, self.username, )
		QueryHandler.execute(query, variables)
		self.expected_result_dict = {"friends": {}, 
									"anonymous": {}}
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
	
	def test_user_match_reregistration(self):
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

		reregistration_response = json.loads(requests.post(self._register_url, data=payload).content)
		assert reregistration_response['status'] == settings.STATUS_200


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
		assert record["device_token"] == self._token
		assert record["token_type"] == self._token_type
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
			assert not record['device_token']
			assert record['device_id']        

	def tearDown(self):
		test_utils.delete_user(username = self._username)
		self.delete_users_match()
		self.delete_match()

class SetAndroidDeviceTokenReturnUserMatchesTest(SetDeviceTokenReturnUserMatchesTest, unittest.TestCase, ):
	_set_url = tornado_local_address + "/set_android_token_and_return_user_matches"
	_unset_url = tornado_local_address + "/remove_android_token"
	_token_type = settings.TOKEN_ANDROID_TYPE
	

class IOSSetUserDeviceIdReturnUserMatchesTests(SetDeviceTokenReturnUserMatchesTest, unittest.TestCase):
	_set_url = tornado_local_address + "/set_ios_token_and_return_user_matches"
	_unset_url = None
	_token_type = settings.TOKEN_IOS_TYPE

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
		assert record['show_location'] == 'a'

		payload = {'username': self._username, 'password': self._password, "show_location_status": "false"}
		payload.update(extra_params_dict)
		response = json.loads(requests.post(self._set_location_privacy_url, data=payload).content)
		assert response['status'] == settings.STATUS_200

		record = test_utils.select_user(username = self._username)[0]
		assert record['username'] == self._username
		assert record['show_location'] == 'n'

	def test_show_location_status(self):
		payload = {"username": self._username, "password": self._password, "show_location_status": "false"}
		payload.update(extra_params_dict)
		response = json.loads(requests.post(self._set_location_privacy_url, data=payload).content)
		assert response['status'] == settings.STATUS_200

		record = test_utils.select_user(username = self._username)[0]
		assert record['username'] == self._username
		assert record['show_location'] == 'n'

		payload = {"username": self._username, "password": self._password, "show_location_status": "true"}
		payload.update(extra_params_dict)
		response = json.loads(requests.post(self._set_location_privacy_url, data=payload).content)
		assert response['status'] == settings.STATUS_200

		record = test_utils.select_user(username = self._username)[0]
		assert record['username'] == self._username
		assert record['show_location'] == 'a'

	def tearDown(self):
		test_utils.delete_user(username = self._username)

class PushNotifcationsTest(unittest.TestCase):
	#TO-DO Write test for users retrieval 
	_push_notification_url = tornado_local_address + "/notify_event"
	_sport_code = "1"
	_event_code = "1"
	_match_id = "1"
	_league_id = "1"
	_payload = {"s": _sport_code, "e": _event_code, "m": _match_id, "tt": "test", "bt": "test", "l": _league_id}

	def test_notify_event_and_storage(self):
		response = json.loads(requests.post(self._push_notification_url, data=json.dumps(self._payload)).content)
		assert response['status'] == settings.STATUS_200

	def test_send_notification(self):
		from common.notification_handler import NotificationHandler
		match_id = self._payload['m'].strip() + "|" + self._payload['l'].strip()
		nh = NotificationHandler(match_id, self._payload).handle_notification()
		query = " SELECT * FROM notifications WHERE match_id = %s "
		variables = (match_id,)
		result = QueryHandler.get_results(query, (variables,))
		assert result[0]['notification'] == self._payload        

	def tearDown(self):
		query = " DELETE FROM notifications WHERE match_id = %s;"
		match_id = match_league_id = self._payload['m'].strip() + "|" + self._payload['l'].strip()
		variables = (match_id,)
		QueryHandler.execute(query, variables)

class ApnsHandlerTest(unittest.TestCase):
	def test_singleton(self):
		from common.notification_handler import ApnsHandler
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
		ids = tuple(list(set(map(lambda x: x['id'], self._matches_1 + self._matches_2))))  
		query = " DELETE FROM matches WHERE id IN {};".format(ids)
		QueryHandler.execute(query, (ids,))

class SetUserInfoTest(unittest.TestCase):
	_set_user_info_url = tornado_local_address + "/set_user_info"
	_username = "test"
	_name = "test"
	_phone_number = "911"
	_password = "test"
	_token = "test_token"
	
	def setUp(self):
		test_utils.delete_user(username = self._username)
		test_utils.create_user(username = self._username, password = self._password, phone_number = self._phone_number)

	def test_add_user_info(self):
		payload = {"username": self._username, "password": self._password, "name": self._name}
		payload.update(extra_params_dict)
		response = json.loads(requests.post(self._set_user_info_url, data=payload).content)
		assert response['status'] == settings.STATUS_200

		record = test_utils.select_user(username = self._username)[0]
		assert record['username'] == self._username
		assert record['name'] == self._name

		payload = {"username": self._username, "password": self._password}
		payload.update(extra_params_dict)
		response = json.loads(requests.post(self._set_user_info_url, data=payload).content)
		assert response['status'] == settings.STATUS_200

		record = test_utils.select_user(username = self._username)[0]
		assert record['username'] == self._username
		assert record['name'] == settings.DEFAULT_USER_NAME

	def tearDown(self):
		test_utils.delete_user(username = self._username)

class GetReferralCodeTest(unittest.TestCase):
	_get_referral_code_url = tornado_local_address + "/get_referral_code"
	_username = "test"
	_phone_number = "911"
	_password = "test"

	def setUp(self):
		test_utils.delete_user(username = self._username)
		test_utils.create_user(username = self._username, password = self._password, phone_number = self._phone_number)



	def test_get_user_referral_code(self):
		payload = {"username": self._username, "password": self._password}
		payload.update(extra_params_dict)
		response = json.loads(requests.post(self._get_referral_code_url, data=payload).content)
		assert response['status'] == settings.STATUS_200
		referral_code = response['referral_code']

		record = test_utils.select_user(username = self._username)[0]
		assert record['username'] == self._username
		assert record['referral_code'] == response['referral_code']
		referral_code = record['referral_code']

		payload = {"username": self._username, "password": self._password}
		payload.update(extra_params_dict)
		response = json.loads(requests.post(self._get_referral_code_url, data=payload).content)
		assert referral_code == response['referral_code']



	def tearDown(self):
		test_utils.delete_user(username = self._username)


class RedeemReferralCodesAndCouponsTest(unittest.TestCase):
	_redeem_code_url = tornado_local_address + "/redeem_code"

	_get_referral_code_url = tornado_local_address + "/get_referral_code"

	_username = "test"
	_phone_number = "911"
	_password = "test"

	_friends_username = "test1"
	_friends_password = "password"
	_friends_phone_number = "912"

	_coupon_code = "test_coupon"
	_coupon_code_limit = 1


	def setUp(self):
		test_utils.delete_user(username = self._username)
		test_utils.create_user(username = self._username, password = self._password, phone_number = self._phone_number)
		test_utils.delete_user(username = self._friends_username)
		test_utils.create_user(username = self._friends_username, password = self._friends_password, phone_number = self._friends_phone_number)

		query = "DELETE FROM coupons WHERE code = %s;"
		variables = (self._coupon_code,)        
		QueryHandler.execute(query, variables)

		query = "INSERT INTO coupons (code, coupon_limit) VALUES (%s, %s);"
		variables = (self._coupon_code, self._coupon_code_limit)
		QueryHandler.execute(query, variables)

		query = "DELETE FROM referrals WHERE username = %s;"
		variables = (self._username,)        
		QueryHandler.execute(query, variables)


	def test_friends_referral_code_redemption(self):
		get_referral_code_payload = {"username": self._friends_username, "password": self._friends_password}
		get_referral_code_payload.update(extra_params_dict)
		response = json.loads(requests.post(self._get_referral_code_url, data=get_referral_code_payload).content)
		friends_referral_code = response['referral_code']

		post_referral_code_payload = {"username": self._username, "password": self._password, "referral_code": friends_referral_code}
		post_referral_code_payload.update(extra_params_dict)
		response = json.loads(requests.post(self._redeem_code_url, data=post_referral_code_payload).content)
		assert response['status'] == settings.STATUS_200

		query = "SELECT * FROM referrals WHERE username = %s;"
		variables = (self._username,)
		record = QueryHandler.get_results(query, variables)
		assert record
		assert record[0]['referred_by'] == self._friends_username


	def test_coupon_code_redemption(self):

		post_referral_code_payload = {"username": self._username, "password": self._password, "referral_code": self._coupon_code}
		post_referral_code_payload.update(extra_params_dict)
		response = json.loads(requests.post(self._redeem_code_url, data=post_referral_code_payload).content)
		assert response['status'] == settings.STATUS_200

		query = "SELECT * FROM referrals WHERE username = %s;"
		variables = (self._username,)
		record = QueryHandler.get_results(query, variables)
		assert record
		assert record[0]['referred_by'] == self._coupon_code

	def test_reapplying_referral_failure(self):
		get_referral_code_payload = {"username": self._friends_username, "password": self._friends_password}
		get_referral_code_payload.update(extra_params_dict)
		response = json.loads(requests.post(self._get_referral_code_url, data=get_referral_code_payload).content)
		friends_referral_code = response['referral_code']

		post_referral_code_payload = {"username": self._username, "password": self._password, "referral_code": friends_referral_code}
		post_referral_code_payload.update(extra_params_dict)
		response = json.loads(requests.post(self._redeem_code_url, data=post_referral_code_payload).content)
		assert response['status'] == settings.STATUS_200

		response = json.loads(requests.post(self._redeem_code_url, data=post_referral_code_payload).content)
		assert response['status'] == settings.STATUS_400


	def test_reapplying_code_failure(self):
		post_referral_code_payload = {"username": self._username, "password": self._password, "referral_code": self._coupon_code}
		post_referral_code_payload.update(extra_params_dict)
		response = json.loads(requests.post(self._redeem_code_url, data=post_referral_code_payload).content)
		assert response['status'] == settings.STATUS_200

		response = json.loads(requests.post(self._redeem_code_url, data=post_referral_code_payload).content)
		assert response['status'] == settings.STATUS_400

	def test_coupon_limit_failure(self):
		post_referral_code_payload = {"username": self._username, "password": self._password, "referral_code": self._coupon_code}
		post_referral_code_payload.update(extra_params_dict)
		response = json.loads(requests.post(self._redeem_code_url, data=post_referral_code_payload).content)
		assert response['status'] == settings.STATUS_200

		post_referral_code_payload = {"username": self._username, "password": self._password, "referral_code": self._coupon_code}
		post_referral_code_payload.update(extra_params_dict)
		response = json.loads(requests.post(self._redeem_code_url, data=post_referral_code_payload).content)
		assert response['status'] == settings.STATUS_400

	def test_wrong_code_failure(self):
		wrong_code = 'wrong_code'

		post_referral_code_payload = {"username": self._username, "password": self._password, "referral_code": wrong_code}
		post_referral_code_payload.update(extra_params_dict)
		response = json.loads(requests.post(self._redeem_code_url, data=post_referral_code_payload).content)
		assert response['status'] == settings.STATUS_400


	def tearDown(self):
		test_utils.delete_user(username = self._username)
		test_utils.delete_user(username = self._friends_username)
		query = "DELETE FROM coupons WHERE code = %s;"
		variables = (self._coupon_code, )
		QueryHandler.execute(query, variables)

		query = "DELETE FROM referrals WHERE username = %s;"
		variables = (self._username,)
		QueryHandler.execute(query, variables)

class UsersFriendsWatchingTest(unittest.TestCase):
	_get_friends_watching_url = tornado_local_address + "/friends_watching"

	_username = "test"
	_phone_number = "911"
	_password = "test"

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
		query = " INSERT INTO users_matches (username, match_id) VALUES (%s, %s);"
		variables = (username, match_id,)
		QueryHandler.execute(query, variables)

	def unregister_match(self, username, match_id):
		query = " DELETE FROM users_matches WHERE username = %s AND match_id = %s;"
		variables = (username, match_id,)
		QueryHandler.execute(query, variables)

	def unregister_matches(self, username):
		query = " DELETE FROM users_matches WHERE username = %s;"
		variables = (username,)
		QueryHandler.execute(query, variables)		

	def delete_user_friends(self, username):
		query = "DELETE FROM rosterusers WHERE username=%s;"
		variables = (username,)
		QueryHandler.execute(query, variables)


	def test_retrieve_friends_watching_the_match(self):
		payload = {"username": self._username, "password": self._password, "matches": [self._match_id_1, self._match_id_2]}
		payload.update(extra_params_dict)
		response = json.loads(requests.post(self._get_friends_watching_url, data=payload).content)
		assert response['status'] == settings.STATUS_200
		assert response['matches'][self._match_id_1]
		assert not response['matches'].get(self._match_id_2)

		assert type(response['matches'][self._match_id_1]) == list
		assert len(response['matches'][self._match_id_1]) == 2
		assert self._first_friends_username in response['matches'][self._match_id_1]
		assert self._second_friends_username in response['matches'][self._match_id_1]

		# Case: When one of the friends has unregistered the match

		self.unregister_match(self._second_friends_username, self._match_id_1)
		response = json.loads(requests.post(self._get_friends_watching_url, data=payload).content)
		assert self._first_friends_username in response['matches'][self._match_id_1]
		assert not self._second_friends_username in response['matches'][self._match_id_1]

		# Case: When one of the friends has subscribed to the second match
		self.register_match(self._second_friends_username, self._match_id_2)
		response = json.loads(requests.post(self._get_friends_watching_url, data=payload).content)
		assert self._first_friends_username in response['matches'][self._match_id_1]
		assert self._second_friends_username in response['matches'][self._match_id_2]

		# Case: When first user  is not friends
		self.update_roster_entry(self._first_friends_username, 'N')
		response = json.loads(requests.post(self._get_friends_watching_url, data=payload).content)
		assert not response['matches'].get(self._match_id_1)
		assert self._second_friends_username in response['matches'][self._match_id_2]

	def test_empty_match_list(self):
		payload = {"username": self._username, "password": self._password, "matches": []}
		payload.update(extra_params_dict)
		response = json.loads(requests.post(self._get_friends_watching_url, data=payload).content)
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
