from global_func import QueryHandler, S3Handler
from notification_adapter import NotificationAdapter
from tornado.log import enable_pretty_logging
from tornado.options import options
import ConfigParser
import facebook
import time
import tornado
import tornado.ioloop
import tornado.web
import random
import register
import ConfigParser
from notification_adapter import NotificationAdapter
import os
import requests

config = ConfigParser.ConfigParser()
config.read('config.py')


class LocationHandler(tornado.web.RequestHandler):
    def get(self):
        response = {}
        try:
            user = str(self.get_arguments("user", True)[0])
            longtitude = float(self.get_arguments("lng", True)[0])
            latitude = float(self.get_arguments("lat", True)[0])
            username = str.split(user, "@")[0]
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
		self.password = str(password)

	def authenticate(self):
		query = " SELECT * FROM users WHERE username = %s AND password = %s; "
		variables = (self.username, self.password,)

		record = QueryHandler.get_results(query, variables)

		if record:
			return True
		else:
			return False		

	def handle_creation(self, auth_code):
		if self.is_token_correct(auth_code):
			is_created, password = self.exists()
			if not is_created:
				response, status = self.create_new()
				password = self.password

			else:
				response, status = " User already created ", 200
				self.password = password
		else:
			response, status, password = " Wrong or Expired Token ", 400, None
		self.delete_registered()
		return response, status, password

	def is_token_correct(self, auth_code):
		query = " SELECT * FROM registered_users WHERE username = %s AND authorization_code = %s ;"
		variables = (self.username,auth_code,)

		record = QueryHandler.get_results(query, variables)

		if record and (record[0]['expiration_time'] > int(time.time())):
			is_token_correct = True
		else:
			is_token_correct = False
		return is_token_correct 

	def exists(self):
		query = " SELECT * FROM users WHERE username = %s;"
		variables = (str.split(self.username,'@')[0],)
		user_info = QueryHandler.get_results(query, variables)
		if len(user_info) == 0:
			registered = False
			password = None
		else:
			password = user_info[0]['password']
			registered = True
		return registered, password

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
			print " Response in registering users %s " % response
			return response, status

	def handle_registration(self):
		self.delete_registered()
		response, status = self.register()
		return response, status

	def delete_registered(self):
		print "deleting registered user"
		query = " DELETE FROM registered_users WHERE username = %s ;"
		variables = (self.username,)
		QueryHandler.execute(query, variables)

	def register(self):
		random_integer = random.randint(1000,9999)
		expiration_time = int(time.time()) + int(config.get('registration','expiry_period_sec'))

		query = " INSERT INTO registered_users (username, authorization_code, expiration_time) VALUES ( %s, %s, %s); "
		variables = (self.username, random_integer, expiration_time)
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
			'msg' : str.strip(message),
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

            username = str.split(user_id, "@")[0]

            args = {'fields': 'id,name,email,friends', }
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
            username = str.strip(number) + config.get('xmpp', 'domain')
            user = User(username)
            response['info'], response['status'] = user.handle_registration()
        except Exception, e:
            response['info'] = " Error: %s " % e
            response['status'] = 500
        finally:
            self.write(response)


class CreationHandler(tornado.web.RequestHandler):
    def get(self):
        response = {}
        try:
            phone_number = str(self.get_arguments("phone_number")[0])
            username = str.strip(phone_number) + config.get('xmpp', 'domain')
            auth_code = str(self.get_arguments("auth_code")[0])
            password = int(random.random() * 1000000)
            user = User(username, password)
            response['info'], response['status'], response['password'] = user.handle_creation(auth_code)
        except Exception, e:
            response['info'] = " Error %s " % e
            response['status'] = 500
        finally:
            self.write(response)


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
                                                        , (from_timestamp[0], to_timestamp[0], skip[0], limit[0],))
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
                                                        , (skip[0], limit[0],))
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
                                                        , (
                    from_timestamp[0], to_timestamp[0], group[0], skip[0], limit[0],))
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
                                                        , (from_timestamp[0], to_timestamp[0], user[0], skip[0], limit[0],))
            response['status'] = 200
        except Exception, e:
            response['info'] = " Error: %s" % e
            response['status'] = 500
        self.write(response)


class PubSubEventHandler(tornado.web.RequestHandler):
    def get(self):
        response = {}
        try:
            name = self.get_arguments("name")[0]

        except Exception, e:
            response['message'] = " Internal Server Error "
            response['status'] = 500
        except NameError:
            response['message'] = " Proper parameters not supplied "
            response['status'] = 500
        else:
            response['message'] = "Success"
            response['status'] = 200
        finally:
            self.write(response)


class ProfilePicHandler(tornado.web.RequestHandler):
    def post(self):
        response = {}
        try:
            profile_pic_bucket = config.get('amazon', 'profile_pics_bucket')
            info = self.request.files['file'][0]
            file_name = info['filename']
            image_file = info['body']
            username = self.get_arguments('username')[0]
            password = self.get_arguments('password')[0]
            user = User(username, password)
            if user.authenticate():
                s3 = S3Handler(profile_pic_bucket)
                s3.upload(username, image_file)
                response['status'] = 200
                response['info'] = 'Success'
            else:
                response['status'] = 400
                response['info'] = 'Invalid Credentials'
        except Exception, e:
            response['status'] = 500
            response['info'] = 'error is: %s' % e
        finally:
            self.write(response)


class MediaHandler(tornado.web.RequestHandler):
	def post(self):
		response = {}
		try:
			info = self.request.files['file'][0]
			file_name = "media/" + info['filename']
			image_file = info['body']
			if not os.path.isfile(file_name): 
				media_file = open(file_name, 'w')
				media_file.write(image_file)
			response['status'] = 200 
			response['info'] = 'Success' 
		except Exception, e:
			response['status'] = 500 
			response['info'] = 'error is: %s' % e 
		finally:
			self.write(response)

	def get(self):
		response = {}
		file_name = "media/" + self.get_arguments("filename")[0]
		try:
			if os.path.isfile(file_name):
				f = open(file_name, 'r')
				self.write(f.read())
				f.close()
			else:  
				response['info'] = 'Not Found' 
				response['status'] = 400 
		except Exception, e:
			response['status'] = 500 
			response['info'] = 'error is: %s' % e 
			self.write(response)



class FootballEvents(tornado.web.RequestHandler):
    def post(self):
        response = {}
        event = tornado.escape.json_decode(self.request.body)
        if event:
            NotificationAdapter(event, "Football").notify()


class TennisEvents(tornado.web.RequestHandler):
    def post(self):
        response = {}
        event = tornado.escape.json_decode(self.request.body)
        if event:
            NotificationAdapter(event, "Tennis").notify()


def make_app():
	return tornado.web.Application([
		(r"/messages", ArchiveAcessHandler),
		(r"/groups", GroupsHandler),
		(r"/groups_messages", GroupsMessagesHandler),
		(r"/group_messages", GroupMessagesHandler),
		(r"/user_group", UserGroupMessagesHandler),
		(r"/register", RegistrationHandler),
		(r"/create", CreationHandler),
		(r"/location", LocationHandler),
		(r"/fb_friends", FacebookHandler),
		(r"/profile_pic", ProfilePicHandler),
		(r"/football_notifications", FootballEvents),
		(r"/tennis_notifications", TennisEvents),
		(r"/media", MediaHandler),
	], 
	autoreload = True,
	)

if __name__ == "__main__":
    app = make_app()
    options.log_file_prefix = "tornado_log"
    enable_pretty_logging(options=options)
    app.listen(3000)
    tornado.ioloop.IOLoop.current().start()
