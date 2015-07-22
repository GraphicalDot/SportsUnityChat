import tornado.ioloop
import tornado.web
import psycopg2
from tornado.log import enable_pretty_logging

class QueryHandler:
	@classmethod
	def get_results(cls, query, variables):
		connection = psycopg2.connect("dbname=test host=localhost user=test password=test")
		cursor = connection.cursor(cursor_factory=psycopg2.extras.RealDictCursor)
		return cursor.execute(query, variables)

class ArchiveAcessHandler(tornado.web.RequestHandler):
    def get(self):
    	from_timestamp = str(self.get_arguments("from")) 
        to_timestamp = str(self.get_arguments("to"))
        skip = self.get_arguments("skip", True) or [0]
    	limit = self.get_arguments("limit", True) or [100]
        response = {}
        response['version'] = 0.1
        try:
        	response['info'] = QueryHandler.get_results(" SELECT txt, username FROM archive WHERE timestamp > %s \
                                                        AND timestamp < %s OFFSET %s LIMIT %s; \
                                                        ",(from_timestamp[0], to_timestamp[0], skip[0], limit[0],))
        	response['status'] = 200
        except Exception, e:
            response['info'] = " Error: %s" % e
            response['status'] = 500
        self.write(response)

class GroupsMessageHandler(tornado.web.RequestHandler):
    def get(self):
        from_timestamp = str(self.get_arguments("from")) 
        to_timestamp = str(self.get_arguments("to"))
        skip = self.get_arguments("skip", True) or 0
        limit = self.get_arguments("limit", True) or 100
        response = {}
        response['version'] = 0.1
        try:
            response['info'] = QueryHandler.get_results(" SELECT txt, username FROM archive WHERE timestamp > %s \
                                                        AND timestamp < %s AND bare_peer LIKE '%@conference.mm.io' OFFSET %s LIMIT %s; \
                                                        ",(from_timestamp, to_timestamp, skip, limit,))
            response['status'] = 200
        except Exception, e:
            response['info'] = " Error: %s" % e
            response['status'] = 500
        self.write(response)

class GroupMessageHandler(tornado.web.RequestHandler):
    def get(self):
        from_timestamp = str(self.get_arguments("from")) 
        to_timestamp = str(self.get_arguments("to"))
        skip = self.get_arguments("skip", True) or 0
        group = self.get_arguments("group", True) or 0
        limit = self.get_arguments("limit", True) or 100
        response = {}
        response['version'] = 0.1
        try:
            response['info'] = QueryHandler.get_results(" SELECT txt, username FROM archive WHERE timestamp > %s \
                                                        AND timestamp < %s AND bare_peer LIKE '%s' OFFSET %s LIMIT %s; \
                                                        ",(from_timestamp, to_timestamp, group, skip, limit,))
            response['status'] = 200
        except Exception, e:
            response['info'] = " Error: %s" % e
            response['status'] = 500
        self.write(response)

class UserGroupMessageHandler(tornado.web.RequestHandler):
    def get(self):
        from_timestamp = str(self.get_arguments("from")) 
        to_timestamp = str(self.get_arguments("to"))
        skip = self.get_arguments("skip", True) or 0
        user = self.get_arguments("user", True) or 0
        limit = self.get_arguments("limit", True) or 100
        response = {}
        response['version'] = 0.1
        try:
            response['info'] = QueryHandler.get_results(" SELECT txt, username FROM archive WHERE timestamp > %s \
                                                        AND timestamp < %s AND username LIKE '%s' AND bare_peer \
                                                        LIKE '%@conference.mm.io' OFFSET %s LIMIT %s;\
                                                        ",(from_timestamp, to_timestamp, user, skip, limit,))
            response['status'] = 200
        except Exception, e:
            response['info'] = " Error: %s" % e
            response['status'] = 500
        self.write(response)

application = tornado.web.Application([
    (r"/messages", ArchiveAcessHandler),
    (r"/groups", GroupsMessageHandler),
    (r"/group", GroupMessageHandler),
    (r"/user_group", UserGroupMessageHandler),
], 
autoreload = True,
)

if __name__ == "__main__":
    enable_pretty_logging()
    application.listen(3000)
    tornado.ioloop.IOLoop.current().start()