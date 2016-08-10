import json
import tornado
import tornado.web
import tornado.escape
import settings
import urlparse

from dateutil import parser
from functools import wraps
from tornado.web import asynchronous

from common.funcs import QueryHandler
from models.s3_object import S3Object
from utils import upload_s3_object


def execution_trace(arguments):
    def true_decorator(func):
        @wraps(func)
        def wrapped(self, *args, **kwargs):
            result = None
            if arguments:
                for arg in arguments:
                    if not self.get_argument(arg, None):
                        result =  {"status": settings.STATUS_400, "info": settings.MISSING_ARGUMENT_ERROR.format(arg)}
                        break

            if not result:
                result = func(self, *args, **kwargs)
            self.write(result)
            return
        return wrapped
    return true_decorator


class NewsConsoleLogin(tornado.web.RequestHandler):

    @execution_trace(['username', 'password'])
    def post(self):
        response = {}
        username = str(self.get_argument('username'))
        password = str(self.get_argument('password'))
        try:
            query = "SELECT role FROM content_writers WHERE username=%s AND password=%s;"
            variables = (username, password)
            user = QueryHandler.get_results(query, variables)
            response.update({'status': settings.STATUS_200, 'info': settings.SUCCESS_RESPONSE,
                             'user_role': user[0]['role']} if user
                            else {'status': settings.STATUS_404, 'info': settings.BAD_AUTHENTICATION_ERROR})
        except Exception, e:
            response.update({'status': settings.STATUS_500, 'info': 'ERROR: %s' % e})
        finally:
            return response


class NewsConsoleAddUser(tornado.web.RequestHandler):

    @execution_trace(['username', 'password', 'user_role'])
    def post(self):
        response = {}
        username = str(self.get_argument('username'))
        password = str(self.get_argument('password'))
        role = str(self.get_argument('user_role'))
        try:
            query = "SELECT writer_id FROM content_writers WHERE username=%s;"
            variables = (username,)
            user = QueryHandler.get_results(query, variables)
            if user:
                response.update({'status': settings.STATUS_409, 'info': settings.USER_ALEADY_EXISTS})
            else:
                query = "INSERT INTO content_writers (username, password, role) VALUES (%s, %s, %s);"
                variables = (username, password, role)
                QueryHandler.execute(query, variables)
                response.update({'status': settings.STATUS_200, 'info': settings.SUCCESS_RESPONSE})
        except Exception, e:
            response.update({'status': settings.STATUS_500, 'info': 'ERROR: %s' % e})
        finally:
            return response


class NewsConsoleUploadS3Object(tornado.web.RequestHandler):

    @execution_trace(['name', 'content', 'type'])
    def post(self):
        response = {}
        name = str(self.get_argument('name'))
        content = self.get_argument('content')
        type = str(self.get_argument('type'))

        try:
            response = upload_s3_object(name, content, type)
        except Exception, e:
            response.update({'status': settings.STATUS_500, 'info': 'ERROR: %s' % e})
        finally:
            return response


class NewsConsoleAddCuratedArticle(tornado.web.RequestHandler):

    def update_database(self):
        query = "INSERT INTO curated_articles (article_headline, article_content, article_image, article_poll_question, " \
                "article_ice_breaker_image, article_sport_type, article_publish_date, article_stats, article_memes, " \
                "article_state) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s);"
        variables = (self.headline, self.news_content, self.news_image_link, self.poll_question, self.ice_breaker_link,
                     self.sport_type, self.publish_date, self.stats, self.memes, self.article_state)
        QueryHandler.execute(query, variables)


    @execution_trace(['headline', 'news_image_name', 'news_image_content', 'news_content', 'ice_breaker_name',
                      'ice_breaker_content', 'poll_question', 'notification_content',
                      'sport_type', 'state'])
    def post(self):
        response = {}
        self.headline = str(self.get_argument('headline'))
        self.news_image_name = str(self.get_argument('news_image_name'))
        self.news_image_content = self.get_argument('news_image_content')
        self.news_content = str(self.get_argument('news_content'))
        self.ice_breaker_image_name = str(self.get_argument('ice_breaker_name'))
        self.ice_breaker_image_content = self.get_argument('ice_breaker_content')
        self.poll_question = str(self.get_argument('poll_question'))
        self.notification_content = str(self.get_argument('notification_content'))
        self.sport_type = str(self.get_argument('sport_type'))
        self.stats = self.get_arguments('stats')
        self.memes = self.get_arguments('memes')
        self.publish_date = parser.parse(str(self.get_argument('publish_date')))
        self.article_state = str(self.get_argument('state'))

        try:
            response = upload_s3_object(self.news_image_name, self.news_image_content, 'news_image')
            if response['status'] == settings.STATUS_200:
                self.news_image_link = response['link']
                response = upload_s3_object(self.ice_breaker_image_name, self.ice_breaker_image_content, 'ice_breaker_image')

                if response['status'] == settings.STATUS_200:
                    self.ice_breaker_link = response['link']
                    self.update_database()
                    response.update({'status': settings.STATUS_200, 'info': settings.SUCCESS_RESPONSE})

        except Exception, e:
            response.update({'status': settings.STATUS_500, 'info': 'ERROR: %s' % e})
        finally:
            return response


class NewsConsoleFetchArticles(tornado.web.RequestHandler):

    def get(self):
        response = {}
        try:
            filter_field = str(self.get_argument('filter_field', 'created_at'))
            order = str(self.get_argument('order', 'ASC'))
            query = "SELECT article_id, article_headline, article_sport_type, to_char(article_publish_date, 'DD MM YYYY') as publish_date, article_state FROM " \
                    "curated_articles " + "ORDER BY %s %s;" % (filter_field, order)
            articles = QueryHandler.get_results(query)
            response.update({'status': settings.STATUS_200, 'info': settings.SUCCESS_RESPONSE, 'articles': json.dumps(articles)})
        except Exception, e:
            response.update({'status': settings.STATUS_500, 'info': 'ERROR: %s' % e})
        finally:
            self.write(response)


class NewsConsoleEditArticle(tornado.web.RequestHandler):

    @execution_trace(['article_id'])
    def get(self):
        response = {}
        self.article_id = str(self.get_argument('article_id'))
        try:
            query = "SELECT article_id, article_headline, article_content, article_image, article_poll_question, " \
                    "article_ice_breaker_image, article_sport_type, to_char(article_publish_date, 'DD MM YYYY') as publish_date, article_stats, article_memes, " \
                    "article_state FROM curated_articles WHERE article_id = %s;"
            variables = (self.article_id,)
            article = QueryHandler.get_results(query, variables)
            response.update({'status': settings.STATUS_200, 'info': settings.SUCCESS_RESPONSE, 'article': json.dumps(article)})
        except Exception, e:
            response.update({'status': settings.STATUS_500, 'info': 'ERROR: %s' % e})
        finally:
            return response

    # def post(self):
    #     response = {}
    #     try:
    #         arguments = urlparse.parse_qs(self.request.body)
    #         if arguments.has_key('article_id'):
    #             self.article_id = arguments.pop('article_id')[0]
    #             query = "UPDATE curated_articles SET " + map(arguments)
    #
    #         else:
    #             response.update({'status': settings.STATUS_400, 'info': settings.MISSING_ARGUMENT_ERROR.format('article_id')})
    #     except Exception, e:
    #         response.update({'status': settings.STATUS_500, 'info': 'ERROR: %s' % e})
    #     finally:
    #         return response
