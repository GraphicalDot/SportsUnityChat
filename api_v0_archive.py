#TO-DO non blocking database wrapper
import random
import tornado.ioloop
import tornado.web
import psycopg2
from tornado.log import enable_pretty_logging
import tornado
import psycopg2.extras
import requests
from IPython import embed
from requests.auth import HTTPBasicAuth
import facebook
import ConfigParser
import subprocess
import register
import ConfigParser
config = ConfigParser.ConfigParser()
config.read('config.py')

class LocationHandler(tornado.web.RequestHandler):
	def get(self):
		response = {}
		try:
			user = str(self.get_arguments("user", True)[0])
			longtitude = float(self.get_arguments("lng", True)[0])
			latitude = float(self.get_arguments("lat", True)[0])
			username = str.split(user,"@")[0]
			query = " UPDATE users SET lat = %s, lng = %s " \
					" WHERE username = %s; "
			QueryHandler.execute(query, (latitude, longtitude, username))
		except Exception, e:
			response["info"] = " Error %s " % e
			response["status"] = 500
		else:
			response['message'] = "Success"
			response["status"] = 200
		finally:
			self.write(response)

class User:
	def __init__(self, username, password = None):
		self.username = username
		self.password = password

	def exists(self):
		query = " SELECT * FROM users WHERE username = %s;"
		variables = (str.split(self.username,'@')[0],)
		user_info = QueryHandler.get_results(query, variables)
		if len(user_info) == 0:
			registered = False
		else:
			self.password = user_info[0]['password']
			registered = True
		return registered

	def create_new(self):
		try:
			registration = register.Register(self.username, self.password)
			registration.register_plugin('xep_0030') 
			registration.register_plugin('xep_0004') 
			registration.register_plugin('xep_0066') 
			registration.register_plugin('xep_0077') 
			registration['xep_0077'].force_registration = True
			if registration.connect(('localhost', 5222)):
				registration.process(block=True)
				response, status = "Success", 200
			else:
				response, status = "Failed registration", 500
		except Exception, e:
			response, status = " %s " % e, 500
		finally:
			return response, status

	def handle_registration(self):
		if self.exists():
			response, status = " User already registered ", 200
		else:
			response, status = self.register()
		return response, status

	def register(self):
		random_integer = int(random.random()*10000) 
		query = " INSERT INTO registered_users (username, authorization_code) VALUES ( %s, %s); "
		variables = (self.username, random_integer,)
		try:
			QueryHandler.execute(query, variables)
			return self.send_message(random_integer)
		except Exception, e:
			return " Error while sending message : % s" % e, 500

	def send_message(self, random_integer):
		number = str.split(self.username,'@')[0]
		message = config.get('database','message') + "  " + str(random_integer)
		payload = {
			'method' : 'SendMessage',
			'send_to' : str.strip(number),
			'msg' : str.strip(message) ,
			'msg_type' : 'TEXT',
			'userid' : config.get('database','gupshup_id'),
			'auth_scheme' : 'plain',
			'password' : config.get('database','gupshup_password'),
			'v' : '1.1',
			'format' : 'text',
		}
		response = requests.get(config.get('database','message_gateway'), params=payload)
		response = str.split(str(response.text),'|')
		if str.strip(str.lower(response[0])) == "success":
			return "Success", 200
		else:
			error = response[2] 
			return error, 500


class FacebookHandler(tornado.web.RequestHandler):
	def get(self):
		response = {}
		try:
			fb_id = str(self.get_arguments("fb_id")[0])
			token = str(self.get_arguments("token")[0])
			user_id = str(self.get_arguments('id')[0])

			username = str.split(user_id,"@")[0]

			args = {'fields' : 'id,name,email,friends', }
			graph = facebook.GraphAPI(token)
			fb_json = graph.get_object('me', **args)

			fb_name = fb_json['name']
			
			self.set_fb_details(fb_id, username, fb_name)

			friends_details = self.get_friends_id(fb_json['friends']['data'])
		# TO-DO explicit error messages
		except Exception, e:
			response['info'] = " Error : % s " % e 
			response['status'] = 500
		else:
			response['list'] = friends_details
			response['status'] = 200
		finally:
			self.write(response)

	def get_friends_id(self, friends_details):
		friends_id_name = []
		for friend_detail in friends_details:
			friend_id_name = {}
			query = " SELECT * FROM users WHERE fb_id = %s;"
			variables = (friend_detail['id'],)
			results = QueryHandler.get_results(query, variables)
			if len(results) > 0:
				friend_id_name['id'] = results['username'] + '@mm.io'
				friend_id_name['fb_name'] = results['fb_name'] + '@mm.io'
				friends_id_name.append(friend_id_name)
		return friends_id_name


	def set_fb_details(self, fb_id, username, fb_name):
		query = " UPDATE users SET fb_id = %s, fb_name = %s WHERE username = %s ;"
		variables = (fb_id, fb_name, username)
		QueryHandler.execute(query, variables)

class RegistrationHandler(tornado.web.RequestHandler):
	def get(self):
		response = {}
		try:
			number = str(self.get_arguments("phone_number")[0])
			username = str.strip(number) + "@mm.io"
			user = User(username)
			response['info'], response['status'] = user.handle_registration()
		except Exception, e:
			response['info'] = " Error: %s " % e
			response['status'] = 500
		finally:
			self.write(response)
			
class AuthorizationHandler(tornado.web.RequestHandler):
	def get(self):
		try:
			pass
		except Exception, e:
			pass
		else:
			pass
		finally:
			pass

class QueryHandler:
	@classmethod
	def get_results(cls, query, variables):
		connection = psycopg2.connect("dbname=test host=localhost user=test password=test")
		cursor = connection.cursor(cursor_factory=psycopg2.extras.RealDictCursor)
		print(cursor.mogrify(query, variables))
		cursor.execute(query, variables)
		results = cursor.fetchall()
		connection.commit()
		cursor.close()
		return results

	@classmethod
	def execute(cls, query, variables):
		connection = psycopg2.connect("dbname=test host=localhost user=test password=test")
		cursor = connection.cursor()
		print(cursor.mogrify(query, variables))
		cursor.execute(query, variables)
		connection.commit()
		cursor.close()

class ArchiveAcessHandler(tornado.web.RequestHandler):
	def get(self):
		from_timestamp = self.get_arguments("from") 
		to_timestamp = self.get_arguments("to")
		skip = self.get_arguments("skip", True) or [0]
		limit = self.get_arguments("limit", True) or [100]
		response = {}
		response['version'] = 0.1
		try:
			response['info'] = QueryHandler.get_results(" SELECT txt, username FROM archive WHERE timestamp > %s " \
														" AND timestamp < %s OFFSET %s LIMIT %s; " \
														,(from_timestamp[0], to_timestamp[0], skip[0], limit[0],))
			response['status'] = 200
		except Exception, e:
			print(e)
			response['info'] = " Error: %s" % e
			response['status'] = 500
		self.write(response)

class GroupsHandler(tornado.web.RequestHandler):
	def get(self):
		skip = self.get_arguments("skip", True) or [0]
		limit = self.get_arguments("limit", True) or [100]
		response = {}
		response['version'] = 0.1
		try:
			response['info'] = QueryHandler.get_results(" SELECT name FROM muc_room OFFSET %s LIMIT %s; " \
														,(skip[0], limit[0],))
			response['status'] = 200
		except Exception, e:
			response['info'] = " Error: %s" % e
			response['status'] = 500
		self.write(response)

class GroupsMessagesHandler(tornado.web.RequestHandler):
	def get(self):
		from_timestamp = self.get_arguments("from") 
		to_timestamp = self.get_arguments("to")
		skip = self.get_arguments("skip", True) or [0]
		limit = self.get_arguments("limit", True) or [100]
		response = {}
		response['version'] = 0.1
		try:
			response['info'] = QueryHandler.get_results(" SELECT txt, username FROM archive WHERE timestamp > %s " \
														" AND timestamp < %s AND bare_peer LIKE '%%@conference.mm.io' " \
														" OFFSET %s LIMIT %s; ", \
														(from_timestamp[0], to_timestamp[0], skip[0], limit[0],))
			response['status'] = 200
		except Exception, e:
			response['info'] = " Error: %s" % e
			response['status'] = 500
		self.write(response)

class GroupMessagesHandler(tornado.web.RequestHandler):
	def get(self):
		from_timestamp = self.get_arguments("from") 
		to_timestamp = self.get_arguments("to")
		skip = self.get_arguments("skip", True) or [0]
		group = self.get_arguments("group", True) or ["test@conference.mm.io"]
		limit = self.get_arguments("limit", True) or [100]
		response = {}
		response['version'] = 0.1
		try:
			response['info'] = QueryHandler.get_results(" SELECT txt, username FROM archive WHERE timestamp > %s " \
														" AND timestamp < %s AND bare_peer LIKE %s OFFSET %s LIMIT %s;" \
														,(from_timestamp[0], to_timestamp[0], group[0], skip[0], limit[0],))
			response['status'] = 200
		except Exception, e:
			response['info'] = " Error: %s" % e
			response['status'] = 500
		self.write(response)

class UserGroupMessagesHandler(tornado.web.RequestHandler):
	def get(self):
		from_timestamp = self.get_arguments("from") 
		to_timestamp = self.get_arguments("to")
		skip = self.get_arguments("skip", True) or [0]
		user = self.get_arguments("user", True) or ['satish@mm.io']
		limit = self.get_arguments("limit", True) or [100]
		response = {}
		response['version'] = 0.1
		try:
			response['info'] = QueryHandler.get_results(" SELECT txt, username FROM archive WHERE timestamp > %s " \
														" AND timestamp < %s AND username LIKE %s AND bare_peer " \
														" LIKE '%%@conference.mm.io' OFFSET %s LIMIT %s; " \
														,(from_timestamp[0], to_timestamp[0], user[0], skip[0], limit[0],))
			response['status'] = 200
		except Exception, e:
			response['info'] = " Error: %s" % e
			response['status'] = 500
		self.write(response)

def make_app():
	return tornado.web.Application([
		(r"/messages", ArchiveAcessHandler),
		(r"/groups", GroupsHandler),
		(r"/groups_messages", GroupsMessagesHandler),
		(r"/group_messages", GroupMessagesHandler),
		(r"/user_group", UserGroupMessagesHandler),
		(r"/register", RegistrationHandler),
		(r"/authorize", AuthorizationHandler),
		(r"/location", LocationHandler),
		(r"/fb_friends", FacebookHandler),
	], 
	autoreload = True,
	)


if __name__ == "__main__":
	tornado.options.parse_config_file('config.py')
	app = make_app()
	enable_pretty_logging()
	app.listen(3000)
	tornado.ioloop.IOLoop.current().start()
