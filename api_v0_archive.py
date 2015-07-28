import tornado.ioloop
import tornado.web
import psycopg2
from tornado.log import enable_pretty_logging
import tornado
import psycopg2.extras
import requests
from requests.auth import HTTPBasicAuth

class UserHandler:
	def post(self):
		user = self.get_arguments("user", True)
		friends = self.get_arguments("friends", True)

class LocationHandler(tornado.web.RequestHandler):
	def get(self):
		response = {}
		try:
			user = str(self.get_arguments("user", True)[0])
			longtitude = int(self.get_arguments("long", True)[0])
			latitude = int(self.get_arguments("lat", True)[0])
			username = str.split(user,"@")[0]
			query = " UPDATE users SET lat = %s, long = %s " \
					" WHERE username = %s; "
			QueryHandler.get_results(query, (latitude, longtitude, username))
		except TypeError, e:
			response["info"] = " Error: % s " % e
			response["status"] = 400
		except Exception, e:
			response["info"] = " Error %s " % e
			response["status"] = 500
		else:
			response['message'] = "Success"
			response["status"] = 200
		finally:
			self.write(response)

class RegistrationHandler:
	def get(self):
		try:
			fb_id = self.get_arguments("fb_id")[0]
			server = "localhost"
			virtualhost = "mm.io"
			url = "http://%s:5222/admin/server/%s/user/" % (server, virtualhost)
			auth = HTTPBasicAuth("user", "password")
			data = {
			    'newusername': fb_id,
			    'newuserpassword': "new_password",
			    'addnewuser': "Add User"
			}
			resp = requests.post(url, data=data, auth=auth)
			response['status'] = 200
			response["password"] = "newuserpassword"
		except Exception, e:
			response['status'] = 500
		self.write(response)

class QueryHandler:
	@classmethod
	def get_results(cls, query, variables):
		connection = psycopg2.connect("dbname=test host=localhost user=test password=test")
		cursor = connection.cursor(cursor_factory=psycopg2.extras.RealDictCursor)
		print(cursor.mogrify(query, variables))
		cursor.execute(query, variables)
		return cursor.fetchall()

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
	(r"/register", RegistrationHandler),
	(r"/location", LocationHandler),
], 
autoreload = True,
)

if __name__ == "__main__":
	enable_pretty_logging()
	application.listen(3000)
	tornado.ioloop.IOLoop.current().start()
