# -*- coding: UTF-8 -*-

import base64
import datetime
import json
import psycopg2.extras
import requests
import tornado
import tornado.escape
import tornado.web
import threading
import urlparse
from dateutil import parser
from psycopg2 import IntegrityError
from tornado.web import MissingArgumentError

import settings
from utils import ConsoleS3Object
from common.custom_error import BadAuthentication, BadInfoSuppliedError, DuplicateKeyError, InvalidBucketRequest, \
    KeyAlreadyExists, InvalidKeyError
from common.funcs import QueryHandler
from models.discussion import Discussion

class BaseRequestHandler(tornado.web.RequestHandler):
    def set_default_headers(self):
        self.set_header("Access-Control-Allow-Origin", "*")
        self.set_header("Access-Control-Allow-Headers", "x-requested-with")
        self.set_header('Access-Control-Allow-Methods', 'POST, GET, OPTIONS')

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

        if error_type in [BadInfoSuppliedError, MissingArgumentError, ValueError, KeyError, IntegrityError,
                          InvalidBucketRequest, InvalidKeyError]:
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
        bucket_name = settings.articles_BUCKETS.get(type, None)
        image_name = base64.encodestring(name.strip(''))
        s3_object = ConsoleS3Object(object_type=type, image_name=image_name, content=content, acl='public-read')
        s3_object.handle_upload()
        s3_object_link = "{}/{}/{}".format(s3_object.client.meta.endpoint_url, s3_object.bucket_name, image_name)
        response.update({'status': settings.STATUS_200, 'info': settings.SUCCESS_RESPONSE, 'link': s3_object_link})
        self.write(response)


class NewsConsoleAddCuratedArticle(BaseRequestHandler):

    def update_database(self):
        query = "INSERT INTO articles (article_headline, article_content, article_poll_question, article_notification_content," \
                "article_sport_type, article_stats, article_memes, article_state, article_writer) " \
                "VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s) RETURNING article_id;"
        variables = (self.article_headline, self.article_content, self.article_poll_question, self.article_notification_content,
                     self.article_sport_type, self.article_stats, self.article_memes, self.article_state, self.article_writer)
        result = QueryHandler.get_results(query, variables)
        return result[0]['article_id']

    def upload_images(self):
        s3_object = ConsoleS3Object(object_type='news_image', image_name=str(self.article_id) + '.' + self.article_image_type,
                        content=self.article_image, acl='public-read')
        s3_object.handle_upload()
        self.article_image_link = "{}/{}/{}".format(s3_object.client.meta.endpoint_url, s3_object.bucket_name, s3_object.name)

        s3_object = ConsoleS3Object(object_type='ice_breaker_image', image_name=str(self.article_id) + '.' + self.article_ice_breaker_image_type,
                        content=self.article_ice_breaker_image, acl='public-read')
        s3_object.handle_upload()
        self.ice_breaker_link = "{}/{}/{}".format(s3_object.client.meta.endpoint_url, s3_object.bucket_name, s3_object.name)

        query = "UPDATE articles SET article_image = %s, article_ice_breaker_image = %s WHERE article_id = %s;"
        variables = (self.article_image_link, self.ice_breaker_link, self.article_id)
        QueryHandler.execute(query, variables)

    def post(self):
        response = {}
        self.article_writer = str(self.get_argument('username'))
        self.article_headline = str(self.get_argument('article_headline'))
        self.article_content = str(self.get_argument('article_content'))

        article_image_file = self.request.files['article_image'][0]
        self.article_image = article_image_file['body']
        self.article_image_type = str(article_image_file['content_type']).split('/')[1]
        if self.article_image_type == 'unknown':
            self.article_image_type = 'jpeg'

        ice_breaker_file = self.request.files['article_ice_breaker_image'][0]
        self.article_ice_breaker_image = ice_breaker_file['body']
        self.article_ice_breaker_image_type = str(ice_breaker_file['content_type']).split('/')[1]
        if self.article_ice_breaker_image_type == 'unknown':
            self.article_ice_breaker_image_type = 'jpeg'

        self.article_poll_question = str(self.get_argument('article_poll_question'))
        self.article_notification_content = str(self.get_argument('article_notification_content', self.article_headline))
        self.article_sport_type = str(self.get_argument('article_sport_type'))
        self.article_stats = self.get_arguments('article_stats')
        self.article_memes = self.get_arguments('article_memes')
        self.article_state = str(self.get_argument('article_state'))

        self.article_id = self.update_database()
        self.upload_images()
        response.update({'status': settings.STATUS_200, 'info': settings.SUCCESS_RESPONSE})
        self.write(response)


class NewsConsoleFetchArticles(BaseRequestHandler):

    def get_user_role(self):
        query = "SELECT role FROM content_writers WHERE username = %s;"
        variables = (self.username,)
        result = QueryHandler.get_results(query, variables)
        if not result:
            raise BadInfoSuppliedError('username')
        return result[0]['role']

    def get_query(self):
        query = "SELECT article_id, article_headline, article_sport_type, article_image, to_char(article_publish_date, 'DD MM YYYY') " \
                "as article_publish_date, article_state, article_writer FROM articles "

        query += "WHERE article_id NOT IN (SELECT article_id FROM articles WHERE article_state='Draft' AND article_writer!='%s') " \
                 % self.username if self.is_admin else "WHERE article_writer='%s' " % self.username

        if self.article_sport_type:
            query += " AND article_sport_type = '%s'" % self.article_sport_type

        if self.article_state:
            query += " AND article_state = '%s'" % self.article_state
        query += 'ORDER BY created_at DESC;'
        return query

    def data_validation(self):
        if self.article_sport_type not in ['c', 'f', '']:
            raise BadInfoSuppliedError('article_sport_type')

        if self.article_state not in ['Draft', 'UnPublished', "Published", '']:
            raise BadInfoSuppliedError('article_state')

    def get(self):
        response = {}
        self.username = str(self.get_argument('username'))
        self.article_sport_type = str(self.get_argument('article_sport_type', ''))
        self.article_state = str(self.get_argument('article_state', ''))
        self.is_admin = True if self.get_user_role() == 'admin' else False
        self.data_validation()
        query = self.get_query()
        articles = QueryHandler.get_results(query)
        response.update({'status': settings.STATUS_200, 'info': settings.SUCCESS_RESPONSE, 'articles': json.dumps(articles)})
        self.write(response)


class NewsConsoleGetArticle(BaseRequestHandler):

    def get(self):
        response = {}
        self.article_id = int(self.get_argument('article_id'))
        query = "SELECT article_id, article_headline, article_content, article_image, article_poll_question, " \
                "article_ice_breaker_image, article_sport_type, to_char(article_publish_date, 'DD MM YYYY') as article_publish_date, article_stats, article_memes, " \
                "article_state, article_notification_content, article_writer FROM articles WHERE article_id = %s;"
        variables = (self.article_id,)
        article = QueryHandler.get_results(query, variables)
        if not article:
            raise BadInfoSuppliedError('article_id')
        response.update({'status': settings.STATUS_200, 'info': settings.SUCCESS_RESPONSE, 'article': article[0]})
        self.write(response)


class NewsConsoleEditArticle(BaseRequestHandler):

    def post(self):
        response = {}
        self.article_id = int(self.get_argument('article_id'))
        arguments = urlparse.parse_qs(self.request.body)
        arguments.pop('article_id')
        self.stats = self.get_arguments('article_stats')
        self.memes = self.get_arguments('article_memes')
        self.publish_date = self.get_argument('article_publish_date', None)

        if not set(arguments.keys()).issubset(['article_headline', 'article_content', 'article_image', 'article_poll_question',
                                           'article_ice_breaker_image', 'article_sport_type', 'article_stats', 'article_memes',
                                           'article_state', 'article_notification_content']):
            raise InvalidKeyError

        query = "UPDATE articles SET {} WHERE article_id = {};"
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
        self.article_id = int(self.get_argument('article_id'))
        self.force_deletion = int(self.get_argument('force_delete'))
        if self.force_deletion:
            query = "DELETE FROM articles WHERE article_id = %s;" % self.article_id
            QueryHandler.execute(query)
        else:
            query = "SELECT article_id FROM carousel_articles WHERE article_id=%s;"
            variables = (self.article_id,)
            result = QueryHandler.get_results(query, variables)
            if result:
                response.update({'in_carousel': True})
            else:
                query = "DELETE FROM articles WHERE article_id = %s;" % self.article_id
                QueryHandler.execute(query)
                response.update({'in_carousel': False})
        response.update({'status': settings.STATUS_200, 'info': settings.SUCCESS_RESPONSE})
        self.write(response)


class NewsConsolePublishArticle(BaseRequestHandler):

    def publish_article(self):
        article = self.articles[0]
        article.update({'type': 'published'})
        requests.post(url=settings.articles_POST_URL, data=article)

    def post(self):
        response = {}
        article_id = self.get_argument('article_id')
        query = "UPDATE articles SET article_state='Published', article_publish_date=%s WHERE article_id = %s RETURNING article_id, " \
                "article_headline, article_content, article_image, article_poll_question, article_sport_type, " \
                "to_char(article_publish_date, 'DD/MM/YYYY') as article_publish_date, article_state, article_writer, article_notification_content;"
        variables = (datetime.datetime.now(), article_id,)
        self.articles = QueryHandler.get_results(query, variables)
        if not self.articles:
            raise BadInfoSuppliedError('article_id')
        threading.Thread(group = None, target = self.publish_article, name = None, args = ()).start()
        response.update({'status': settings.STATUS_200, 'info': settings.SUCCESS_RESPONSE, 'article': self.articles[0]})
        self.write(response)


class NewsConsoleGetCarouselArticles(BaseRequestHandler):

    def get(self):
        response = {}
        query = "SELECT article_id, priority FROM carousel_articles;"
        carousel_articles = QueryHandler.get_results(query)

        query = "SELECT article_id, article_headline, article_image, article_sport_type, to_char(article_publish_date, 'DD MM YYYY')"\
                " as article_publish_date, article_writer FROM articles WHERE article_state='Published';"
        published_articles = QueryHandler.get_results(query)
        all_articles = {'carousel': carousel_articles, 'published': published_articles}
        response.update({'status': settings.STATUS_200, 'info': settings.SUCCESS_RESPONSE, 'articles': all_articles})
        self.write(response)


class NewsConsolePostArticlesOnCarousel(BaseRequestHandler):

    def post_carousel_articles(self):
        data = {'articles': self.articles, 'type': 'carousel'}
        requests.post(url=settings.articles_POST_URL, data=json.dumps(data))

    def post(self):
        response = {}
        self.articles = json.loads(self.get_argument('articles'))

        for key, value in self.articles.items():
            query = "WITH upsert AS (UPDATE carousel_articles SET article_id = %s WHERE priority = %s RETURNING id) " \
                    "INSERT INTO carousel_articles (article_id, priority) SELECT %s, %s WHERE NOT EXISTS (SELECT * FROM upsert);"
            variables = (int(value), int(key), int(value), int(key))
            QueryHandler.execute(query, variables)

        threading.Thread(group=None, target=self.post_carousel_articles, name=None, args=()).start()
        response.update({'status': settings.STATUS_200, 'info': settings.SUCCESS_RESPONSE})
        self.write(response)


class GetDiscussionsHandler(BaseRequestHandler):
    def get(self):
        response = {}
        response['info'] = Discussion.get_all()
        response['status'] = settings.STATUS_200
        self.write(response)
