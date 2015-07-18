import tornado.ioloop
import tornado.web
import psycopg2

class QueryHandler:
	@classmethod
	def get_results(cls, query, variables):
		connection = psycopg2.connect("dbname=development host=localhost user=development password=development")
		cursor = connection.cursor()
		return cursor.execute("SELECT * FROM logs;")

class MainHandler(tornado.web.RequestHandler):
    def get(self):
        self.write("Hello, world ")

class ArchiveAcessHandler(tornado.web.RequestHandler):
    def get(self):
    	from_timestamp = self.get_arguments("from_timestamp", True) 
    	to_timestamp = self.get_arguments("to_timestamp", True)
    	example = {}
    	example['info'] = QueryHandler.get_results("","")
    	self.write(example)

application = tornado.web.Application([
    (r"/messages", ArchiveAcessHandler),
    (r"/", MainHandler),
    (r"/", MainHandler),
    (r"/", MainHandler),
], 
autoreload = True)

if __name__ == "__main__":
    application.listen(3000)
    tornado.ioloop.IOLoop.current().start()