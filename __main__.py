from tornado.log import enable_pretty_logging
from tornado.options import options
import logging
import ConfigParser
import tornado
import tornado.ioloop
import tornado.autoreload
import tornado.escape
import tornado.web
config = ConfigParser.ConfigParser()
config.read('config.py')
from routes import urls
import os
import settings
import argparse
import runpy

class Application(tornado.web.Application):
    def __init__(self):
        settings = dict(
            static_path=os.path.join(os.path.dirname(__file__), "static"),
            autoescape=None
        )
        tornado.web.Application.__init__(self, urls, **settings)


def add_templates_for_tornado_watch(watched_files):
    for file_name in watched_files:
        tornado.autoreload.watch(settings.ADMIN_TEMPLATES_PATH + file_name)


def run_server():
    from IPython import embed
    embed()
    app = Application()
    options.log_file_prefix  = "tornado_log"
    enable_pretty_logging(options=options)
    app.listen(int(config.get('tornado', 'listening_port')))
    tornado.autoreload.start()
    add_templates_for_tornado_watch(settings.ADMIN_TEMPLATES)
    tornado.ioloop.IOLoop.current().start()

def run_tests():
    runpy.run_module("tests")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Run tornado server or test.')
    parser.add_argument('--test', dest='accumulate', action='store_const',
                       const=run_tests, default=run_server,
                       help='run tests (default: run tornado server)')

    args = parser.parse_args()
    args.accumulate()
