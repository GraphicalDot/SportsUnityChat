import base64
import json
import threading
import tornado
import tornado.escape
import tornado.web
import requests
import settings
import urlparse

from dateutil import parser
from functools import wraps
from psycopg2 import IntegrityError
from tornado.web import asynchronous, MissingArgumentError

from common.funcs import QueryHandler
from common.custom_error import BadAuthentication, BadInfoSuppliedError, DuplicateKeyError, InvalidBucketRequest, KeyAlreadyExists
from models.s3_object import S3Object
from utils import ConsoleS3Object


class BaseRequestHandler(tornado.web.RequestHandler):

    def write_error(self, status_code, **kwargs):
        response = {}
        error_type, error_object = kwargs['exc_info'][0], kwargs['exc_info'][1]

        try:
            if error_type == IntegrityError:
                response["info"] = self.extract_psycopg2_integrity_error(error_object)
            else:
                try:
                    response["info"] = error_object.log_message
                except:
                    response["info"] = error_object.message
        except Exception, e:
            response["info"] = settings.INTERNAL_SERVER_ERROR

        if error_type in [BadInfoSuppliedError, MissingArgumentError, ValueError, KeyError, IntegrityError, InvalidBucketRequest]:
            if error_type == ValueError:
                response["info"] = "Improper JSON format "
            response['status'] = settings.STATUS_400
        elif error_type in [DuplicateKeyError, KeyAlreadyExists]:
            response['status'] = settings.STATUS_409
        elif error_type == BadAuthentication:
            response['status'] = settings.STATUS_404
        else:
            response['status'] = settings.STATUS_500
        self.write(response)


class NewsConsoleLogin(BaseRequestHandler):

    def post(self):
        response = {}
        username = str(self.get_argument('username'))
        password = str(self.get_argument('password'))
        query = "SELECT role FROM content_writers WHERE username=%s AND password=%s;"
        variables = (username, password)
        user = QueryHandler.get_results(query, variables)
        if not user:
            raise BadAuthentication
        response.update({'status': settings.STATUS_200, 'info': settings.SUCCESS_RESPONSE, 'user_role': user[0]['role']})
        self.write(response)


class NewsConsoleAddUser(BaseRequestHandler):

    def post(self):
        response = {}
        username = str(self.get_argument('username'))
        password = str(self.get_argument('password'))
        role = str(self.get_argument('user_role'))

        query = "INSERT INTO content_writers (username, password, role) SELECT %s, %s, %s WHERE NOT EXISTS " \
                "(SELECT writer_id FROM content_writers WHERE username = %s) RETURNING writer_id;"
        variables = (username, password, role, username)
        user = QueryHandler.get_results(query, variables)
        if not user:
            raise DuplicateKeyError('username')
        response.update({'status': settings.STATUS_200, 'info': settings.SUCCESS_RESPONSE})
        self.write(response)


class NewsConsoleUploadS3Object(BaseRequestHandler):

    def post(self):
        response = {}
        name = str(self.get_argument('name'))
        content = self.get_argument('content')
        type = str(self.get_argument('type'))
        bucket_name = settings.CURATED_ARTICLES_BUCKETS.get(type, None)
        image_name = base64.encodestring(name.strip(''))
        s3_object = ConsoleS3Object(object_type=type, image_name=image_name, content=content, acl='public-read')
        s3_object.handle_upload()
        s3_object_link = "{}/{}/{}".format(s3_object.client.meta.endpoint_url, s3_object.bucket_name, image_name)
        response.update({'status': settings.STATUS_200, 'info': settings.SUCCESS_RESPONSE, 'link': s3_object_link})
        self.write(response)


class NewsConsoleAddCuratedArticle(BaseRequestHandler):

    def update_database(self):
        query = "INSERT INTO curated_articles (article_headline, article_content, article_image, article_poll_question, " \
                "article_ice_breaker_image, article_sport_type, article_publish_date, article_stats, article_memes, " \
                "article_state) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s);"
        variables = (self.headline, self.article_content, self.article_image_link, self.poll_question, self.ice_breaker_link,
                     self.sport_type, self.publish_date, self.stats, self.memes, self.article_state)
        QueryHandler.execute(query, variables)

    def upload_news_ice_breaker_images(self):
        s3_object = ConsoleS3Object(object_type='news_image', image_name=base64.encodestring(self.article_image_name),
                        content=self.article_image_content, acl='public-read')
        s3_object.handle_upload()
        self.article_image_link = "{}/{}/{}".format(s3_object.client.meta.endpoint_url, s3_object.bucket_name, s3_object.name)

        s3_object = ConsoleS3Object(object_type='ice_breaker_image', image_name=base64.encodestring(self.ice_breaker_image_name),
                        content=self.ice_breaker_image_content, acl='public-read')
        s3_object.handle_upload()
        self.ice_breaker_link = "{}/{}/{}".format(s3_object.client.meta.endpoint_url, s3_object.bucket_name, s3_object.name)

    def post(self):
        response = {}
        self.headline = str(self.get_argument('headline'))
        self.article_image_name = str(self.get_argument('article_image_name'))
        self.article_image_content = self.get_argument('article_image_content')
        self.article_content = str(self.get_argument('article_content'))
        self.ice_breaker_image_name = str(self.get_argument('ice_breaker_name'))
        self.ice_breaker_image_content = self.get_argument('ice_breaker_content')
        self.poll_question = str(self.get_argument('poll_question'))
        self.notification_content = str(self.get_argument('notification_content'))
        self.sport_type = str(self.get_argument('sport_type'))
        self.stats = self.get_arguments('stats')
        self.memes = self.get_arguments('memes')
        self.publish_date = parser.parse(str(self.get_argument('publish_date')))
        self.article_state = str(self.get_argument('state'))

        self.upload_news_ice_breaker_images()
        self.update_database()
        response.update({'status': settings.STATUS_200, 'info': settings.SUCCESS_RESPONSE})
        self.write(response)


class NewsConsoleFetchArticles(BaseRequestHandler):

    def get_stmt(self):
        if self.filter_field in ['created_at', 'article_publish_date', 'article_headline']:
            return "ORDER BY %s %s;" % (self.filter_field, self.order)

        elif self.filter_field == 'article_state':
            if not self.article_state:
                raise MissingArgumentError('article_state')
            return "WHERE article_state='%s';" % self.article_state

        elif self.filter_field == 'article_sport_type':
            if not self.article_sport_type:
                raise MissingArgumentError('article_sport_type')
            if self.article_sport_type not in ['cricket', 'football']:
                raise BadInfoSuppliedError('article_sport_type')
            return "WHERE article_sport_type='%s';" % self.article_sport_type

    def get(self):
        response = {}
        self.filter_field = str(self.get_argument('filter_field', 'created_at'))
        self.order = str(self.get_argument('order', 'ASC'))
        self.article_state = self.get_argument('article_state', None)
        self.article_sport_type = self.get_argument('article_sport_type', None)

        if self.filter_field not in ['created_at', 'article_state', 'article_publish_date', 'article_sport_type', 'article_headline']:
            raise BadInfoSuppliedError('filter_field')

        query = "SELECT article_id, article_headline, article_sport_type, to_char(article_publish_date, 'DD MM YYYY') " \
                "as publish_date, article_state FROM curated_articles "
        stmt = self.get_stmt()
        articles = QueryHandler.get_results(query + stmt)
        response.update({'status': settings.STATUS_200, 'info': settings.SUCCESS_RESPONSE, 'articles': json.dumps(articles)})
        self.write(response)


class NewsConsoleEditArticle(BaseRequestHandler):

    def get(self):
        response = {}
        self.article_id = str(self.get_argument('article_id'))
        query = "SELECT article_id, article_headline, article_content, article_image, article_poll_question, " \
                "article_ice_breaker_image, article_sport_type, to_char(article_publish_date, 'DD MM YYYY') as publish_date, article_stats, article_memes, " \
                "article_state FROM curated_articles WHERE article_id = %s;"
        variables = (self.article_id,)
        article = QueryHandler.get_results(query, variables)
        if not article:
            raise BadInfoSuppliedError('article_id')
        response.update({'status': settings.STATUS_200, 'info': settings.SUCCESS_RESPONSE, 'article': article[0]})
        self.write(response)

    def post(self):
        response = {}
        self.article_id = self.get_argument('article_id')
        arguments = urlparse.parse_qs(self.request.body)
        arguments.pop('article_id')
        self.stats = self.get_arguments('article_stats')
        self.memes = self.get_arguments('article_memes')
        self.publish_date = self.get_argument('article_publish_date', None)

        query = "UPDATE curated_articles SET {} WHERE article_id = {};"
        stmt = ''
        for key, value in arguments.items():
            if key in ['article_stats', 'article_memes']:
                stmt += "%s = ARRAY%s, " % (key, value)
            elif key == 'article_publish_date':
                stmt += "%s = to_date('%s', 'DD/MM/YYYY'), " % (key, value[0])
            else:
                stmt += "%s = '%s', " % (key, value[0])

        QueryHandler.execute(query.format(stmt[:-2], self.article_id))
        response.update({'status': settings.STATUS_200, 'info': settings.SUCCESS_RESPONSE})
        self.write(response)


class NewsConsoleDeleteArticle(BaseRequestHandler):

    def post(self):
        response = {}
        self.article_id = self.get_argument('article_id')
        query = "DELETE FROM curated_articles WHERE article_id = %s;"
        variables = (self.article_id,)
        QueryHandler.execute(query, variables)
        response.update({'status': settings.STATUS_200, 'info': settings.SUCCESS_RESPONSE})
        self.write(response)


class NewsConsolePublishArticle(BaseRequestHandler):

    def publish_article(self):
        requests.post(url=settings.PUBLISH_ARTICLES_POST_URL, data={'published_articles': self.articles})

    def post(self):
        response = {}
        article_id = self.get_argument('article_id')
        query = "UPDATE curated_articles SET article_state='Published' WHERE article_id = %s RETURNING article_id, " \
                "article_headline, article_content, article_image, article_poll_question, article_ice_breaker_image, " \
                "article_sport_type, to_char(article_publish_date, 'DD/MM/YYYY') as article_publish_date;"
        variables = (article_id,)
        self.articles = QueryHandler.get_results(query, variables)
        if not self.articles:
            raise BadInfoSuppliedError('article_id')
        threading.Thread(group = None, target = self.publish_article, name = None, args = ()).start()
        response.update({'status': settings.STATUS_200, 'info': settings.SUCCESS_RESPONSE})
        self.write(response)


class NewsConsolePostArticlesOnCarousel(BaseRequestHandler):

    def post_carousel_articles(self):
        requests.post(url=settings.CAROUSEL_ARTICLES_POST_URL, data={'articles': self.articles})

    def post(self):
        response = {}
        self.articles = self.get_arguments('articles')
        if not self.articles:
            raise BadInfoSuppliedError('articles')
        threading.Thread(group=None, target=self.post_carousel_articles, name=None, args=()).start()
        response.update({'status': settings.STATUS_200, 'info': settings.SUCCESS_RESPONSE})
        self.write(response)
