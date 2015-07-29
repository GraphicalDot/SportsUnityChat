import tornado.ioloop
import tornado.web
import psycopg2
from tornado.log import enable_pretty_logging
import tornado
import psycopg2.extras
import requests
from requests.auth import HTTPBasicAuth
import facebook
import ConfigParser
import subprocess
import register

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
	def __init__(self, username, password):
		self.username = username
		self.password = password

	def exists(self):
		query = " SELECT * FROM users WHERE username = %s;"
		variables = (str.split(self.username,'@')[0],)
		return len(QueryHandler.get_results(query, variables)) != 0

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

	def register(self):
		print("Exists or not " + str(self.exists()))
		if self.exists():
			response, status = " User already registered ", 200
		else:
			response, status = self.create_new()
		return response, status

class AuthenticationHandler(tornado.web.RequestHandler):
	def get(self):
		response = {}
		try:
			fb_id = str(self.get_arguments("fb_id")[0])
			token = str(self.get_arguments("token")[0])
			graph = facebook.GraphAPI(token)
			fb_json = graph.get_object('/me/friends')
		except Exception, e:
			response['info'] = " Error : % s " % e 
			response['status'] = 500
		else:
			username = fb_id + "@mm.io"
			user = User(username, token)
			response['info'], response['status'] = user.register()
			response['username'] = username 
			response['list'] = fb_json['data']
			response["password"] = token
		finally:
			self.write(response)

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
		# from_timestamp = self.get_arguments("from") 
		# to_timestamp = self.get_arguments("to")
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

application = tornado.web.Application([
	(r"/messages", ArchiveAcessHandler),
	(r"/groups", GroupsHandler),
	(r"/groups_messages", GroupsMessagesHandler),
	(r"/group_messages", GroupMessagesHandler),
	(r"/user_group", UserGroupMessagesHandler),
	(r"/register", AuthenticationHandler),
	(r"/location", LocationHandler),
], 
autoreload = True,
)

if __name__ == "__main__":
	enable_pretty_logging()
	application.listen(3000)
	tornado.ioloop.IOLoop.current().start()
