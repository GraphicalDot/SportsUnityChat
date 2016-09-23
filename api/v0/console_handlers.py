# -*- coding: UTF-8 -*-

import base64
import boto3
import ConfigParser
import datetime
import json
import psycopg2.extras
import requests
import tornado
import tornado.escape
import tornado.web
import threading
import time
import urlparse
import uuid
from dateutil import parser
from psycopg2 import IntegrityError
from tornado.web import MissingArgumentError

import settings
from utils import ConsoleS3Object, get_group_users
from common.notification_handler import GCMHandler
from common.custom_error import BadAuthentication, BadInfoSuppliedError, DuplicateKeyError, InvalidBucketRequest, \
    KeyAlreadyExists, InvalidKeyError, PushNotificationError
from common.funcs import QueryHandler
from models.discussion import Discussion

config = ConfigParser.ConfigParser()
config.read('config.py')


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
        elif error_type == PushNotificationError:
            response['status'] = settings.STATUS_500
        else:
            response['status'] = settings.STATUS_500
        self.write(response)


class NewsConsoleLogin(BaseRequestHandler):

    def post(self):
        """
        It enables content writers to login to Console.
        Input: {'username': text, 'password': text}
        Output: {'status': 200, 'info': 'Success', 'user_role': 'author/admin'} if success
                {'status': 404, 'info': 'Bad Authentication Info'} if user not authenticated
                {'status': 500, 'info': Exception message} if any other error/exception
        """
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
        """
        It enables console admin to add new user/content writer to the console.
        Input: {'username': text, 'password': text, 'user_role': text}
        Output: {'status': 200, 'info': 'Success'}   If new user is successfully added
                {'status': 409, 'info': 'Error: User already exists with this username'}  if any user with this username already exists
        """
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

        query = "INSERT INTO users(username, password, phone_number, name) VALUES (%s, %s, %s, %s);"
        phone_number = '00' + (uuid.uuid4().hex)[:8]
        variables = (username, password, phone_number, username)
        QueryHandler.execute(query, variables)

        response.update({'status': settings.STATUS_200, 'info': settings.SUCCESS_RESPONSE})
        self.write(response)


class NewsConsoleUploadS3Object(BaseRequestHandler):

    def post(self):
        # TODO: To be modified for stats, memes and other media content
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
                "article_sport_type, article_stats, article_memes, article_state, article_writer, article_group_name) " \
                "VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s) RETURNING article_id;"
        variables = (self.article_headline, self.article_content, self.article_poll_question, self.article_notification_content,
                     self.article_sport_type, self.article_stats, self.article_memes, self.article_state, self.article_writer, self.article_group_name)
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
        """
        It enables content writer to add new curated article.
        It uploads news_image and ice_breaker_image on s3.
        It also updates database for the new article.
        Input: {'article_headline': text, 'article_image': base64, 'article_content': text,
        'article_ice_breaker_image': base64, 'article_poll_question': text, 'article_notification_content': text,
        'article_sport_type': either of [‘c’, ‘f’], 'article_stats': list of stat links, 'article_memes': list of meme links,
        'article_state': either of ['Draft', 'UnPublished', 'Published'], 'username': text}
        Output: {'status': 400, 'info': 'Missing argument'} In case any required argument not provided
                {'status': 409, 'info': 'Bad Request: This Key already exists in the bucket'} In case any of the
                news_image or ice_breaker_image already exists in the bucket
                {'status': 200, 'info': 'Success'} In case article is successfully added to the database
                {'status': 500, 'info': 'Internal Server Error'} In case of any other error
        """
        response = {}

        self.article_writer = str(self.get_argument('username'))
        self.article_headline = str(self.get_argument('article_headline'))
        self.article_content = str(self.get_argument('article_content'))
        self.article_group_name = str(self.get_argument('article_group_name'))
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
        """
        It enables console users to view all curated articles; sorted by creation date, by default
        Input: {'username': text, 'article_sport_type': either of ['c', 'f'], 'article_state': either of ['Draft', 'Published', 'UnPublished']}
        Output: {'status': 200, 'info': 'Success', 'articles': list of articles containing details:
            article_id, article_headline, article_sport_type, article_image, article_publish_date, article_state, arrticle_writer}
        """
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
        """
        It enables console user to fetch all deatils of particular article.
        Input: {'article_id': int}
        Output: {'status': 200, 'info': 'Success', 'article': all fields of the article} In case of success
                {'status': 400, 'info': 'Bad Info Supplied'} In case invalid article_id is provided
                {'status': 500, 'info': 'Internal Server Error'} In case of any other error
        """
        response = {}
        self.article_id = int(self.get_argument('article_id'))
        query = "SELECT article_id, article_headline, article_content, article_image, article_poll_question, " \
                "article_ice_breaker_image, article_sport_type, to_char(article_publish_date, 'DD MM YYYY') as article_publish_date, article_stats, article_memes, " \
                "article_state, article_notification_content, article_writer, article_group_name FROM articles WHERE article_id = %s;"
        variables = (self.article_id,)
        article = QueryHandler.get_results(query, variables)
        if not article:
            raise BadInfoSuppliedError('article_id')
        response.update({'status': settings.STATUS_200, 'info': settings.SUCCESS_RESPONSE, 'article': article[0]})
        self.write(response)


class NewsConsoleEditArticle(BaseRequestHandler):

    def get_previous_image_links(self):
        query = "SELECT article_image, article_ice_breaker_image FROM articles WHERE article_id=%s;" % self.article_id
        article = QueryHandler.get_results(query)
        if not article:
            raise BadInfoSuppliedError('article_id')
        return (article[0]['article_image'], article[0]['article_ice_breaker_image'])

    def delete_old_image_upload_new(self, object_type, bucket, old_link, new_image_body, new_image_type):
        old_image_link = old_link.split(bucket + '/')
        s3_client = boto3.client('s3', aws_access_key_id=config.get('amazon', 'amazon_access_key'),
                     aws_secret_access_key=config.get('amazon', 'amazon_secret_key'))
        s3_client.delete_object(Bucket=bucket, Key=old_image_link[1])
        image_new_link = old_image_link[0] + bucket + '/' + str(self.article_id) + '.' + new_image_type
        ConsoleS3Object(object_type=object_type, image_name=str(self.article_id) + '.' + new_image_type,
                        content=new_image_body, acl='public-read').handle_upload()
        return image_new_link

    def handle_new_image(self, file_type, object_type, bucket, old_link):
        file = self.request.files.get(file_type, [])
        if file:
            file = file[0]
            (image_body, new_image_type) = (file['body'], str(file['content_type']).split('/')[1])
            if new_image_type == 'unknown':
                new_image_type = 'jpeg'
            image_link = self.delete_old_image_upload_new(object_type, bucket, old_link, image_body, new_image_type)
            if object_type == 'news_image':
                self.article_image_new_link = image_link
            elif object_type == 'ice_breaker_image':
                self.ice_breaker_image_new_link = image_link


    def update_database(self):
        query = "UPDATE articles SET article_headline=%s, article_content=%s, article_image=%s, article_poll_question=%s, " \
                "article_ice_breaker_image=%s, article_sport_type=%s, article_stats=%s, article_memes=%s, article_state=%s, " \
                "article_notification_content=%s, article_group_name=%s WHERE article_id=%s;"
        variables = (self.article_headline, self.article_content, self.article_image_new_link, self.article_poll_question,
                     self.ice_breaker_image_new_link, self.article_sport_type, self.article_stats, self.article_memes,
                     self.article_state, self.article_notification_content, self.article_group_name, self.article_id)
        QueryHandler.execute(query, variables)

    def post(self):
        """
        It enables console user to edit details of Drafted/Unpublished article.
        Input: {'article_id': integer, all those fields whose values are updated}
        Output: {'status': 200, 'info': 'Success'} In case of success
                {'status': 400, 'info': 'Bad Key provided'} In case wrong post data provided
                {'status': 400, 'info': 'Missing argument'} In case article_id not provided
                {'status': 500, 'info': 'Internal Server Error'} In case of any other error
        """
        response = {}
        self.article_image_new_link = ''
        self.ice_breaker_image_new_link = ''

        self.article_id = int(self.get_argument('article_id'))
        self.article_group_name = str(self.get_argument('article_group_name'))
        self.article_headline = str(self.get_argument('article_headline'))
        self.article_content = str(self.get_argument('article_content'))
        self.article_poll_question = str(self.get_argument('article_poll_question'))
        self.article_notification_content = str(self.get_argument('article_notification_content'))
        self.article_sport_type = str(self.get_argument('article_sport_type'))
        self.article_stats = self.get_arguments('article_stats')
        self.article_memes = self.get_arguments('article_memes')
        self.article_state = str(self.get_argument('article_state'))
        self.news_bucket = settings.articles_BUCKETS.get('news_image')
        self.ice_breaker_bucket = settings.articles_BUCKETS.get('ice_breaker_image')

        (self.article_image_old_link, self.article_ice_breaker_image_old_link) = self.get_previous_image_links()
        self.article_image_new_link = self.article_image_old_link
        self.ice_breaker_image_new_link = self.article_ice_breaker_image_old_link
        self.handle_new_image(file_type='article_image', object_type='news_image', bucket=self.news_bucket, old_link=self.article_image_old_link)
        self.handle_new_image(file_type='article_ice_breaker_image', object_type='ice_breaker_image',
                              bucket=self.ice_breaker_bucket, old_link=self.article_ice_breaker_image_old_link)
        self.update_database()
        response.update({'status': settings.STATUS_200, 'info': settings.SUCCESS_RESPONSE})
        self.write(response)


class NewsConsoleDeleteArticle(BaseRequestHandler):

    def delete_from_mongo_db(self):
        requests.post(url=settings.DELETE_ARTICLE_FROM_MONGO_URL, data={'article_id': str(self.article_id)})

    def post(self):
        """
        It enables console user to delete any particular article.
        Input: {'article_id': integer, 'force_delete': 1/0}
        Output: {'status': 400, 'info': 'Missing argument'} In case article_id/force_delete parameter not provided
                {'status': 200, 'info': 'Success', 'in_carousel': True} If article is in carousel and not deleted yet;
                {'status': 200, 'info': 'Success', 'in_carousel': False} If article not in carousel and deleted from database
                {'status': 200, 'info': 'Success'} If article was in carousel and now forcefully deleted
                {'status': 500, 'info': 'Internal Server Error'} In case of any other error
        """
        response = {}
        self.article_id = int(self.get_argument('article_id'))
        self.force_deletion = int(self.get_argument('force_delete'))
        if self.force_deletion:
            query = "DELETE FROM articles WHERE article_id = %s;" % self.article_id
            QueryHandler.execute(query)
            threading.Thread(group = None, target = self.delete_from_mongo_db, name = None, args = ()).start()
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
                threading.Thread(group = None, target = self.delete_from_mongo_db, name = None, args = ()).start()
        response.update({'status': settings.STATUS_200, 'info': settings.SUCCESS_RESPONSE})
        self.write(response)


class NewsConsolePublishArticle(BaseRequestHandler):

    def publish_article(self):
        self.article = self.articles[0]
        self.article.update({'type': 'published'})
        response = requests.post(url=settings.PUBLISH_ARTICLE_POST_URL, data=self.article)
        return response


    def notify_user(self):
        self.article = self.articles[0]
        below_text = self.article['article_notification_content']
        top_text = "Breaking News"
        sport_type = '1' if self.article['article_sport_type'] == 'c' else '2'
        event_code = settings.CURATED_NEWS_EVENT_CODE
        payload = {'cn': str(self.article_id), 'tt': top_text, 'bt': below_text, 'e': event_code, 's': sport_type}

        query = "SELECT device_token, token_type FROM users;"
        users = QueryHandler.get_results(query)
        gcm_response = GCMHandler().send_notifications(users, payload)

    def post(self):
        """
        It enables console user to publish any curated news.
        Input: {'article_id': integer}
        Output: {'status': 400, 'info': 'Missing argument'} In case article_id not provided
                {'status': 400, 'info': 'Bad Info Supplied'} In case invalid article_id provided
                {'status': 200, 'info': 'Success', 'article': info of published article with updated state} In case of success
                {'status': 500, 'info': 'Internal Server Error'} In case of any other error
        """
        response = {}
        self.article_id = self.get_argument('article_id')
        query = "UPDATE articles SET article_publish_date= %s WHERE article_id = %s RETURNING article_id, " \
                "article_headline, article_content, article_image, article_poll_question, article_sport_type, " \
                "to_char(article_publish_date, 'Dy, DD Mon YYYY HH:MI:SS') as article_publish_date,  article_state, article_writer, " \
                "article_notification_content, article_group_name;"
        variables = (datetime.datetime.now(), self.article_id,)
        self.articles = QueryHandler.get_results(query, variables)
        if not self.articles:
            raise BadInfoSuppliedError('article_id')
        publish_response = self.publish_article()
        if int(json.loads(publish_response.content)["status"]) == 200:
            query = "UPDATE articles SET article_state='Published'  WHERE article_id = %s;"
            variables = (self.article_id,)
            QueryHandler.execute(query, variables)
            self.notify_user()
            response.update({'status': settings.STATUS_200, 'info': settings.SUCCESS_RESPONSE, 'article': self.articles[0]})
        else:
            response.update({'status': int(json.loads(publish_response)["status"]), 'info': json.loads(publish_response)["info"]})
        self.write(response)


class NewsConsoleGetCarouselArticles(BaseRequestHandler):

    def get(self):
        """
        It enables console user(s) to fetch all published articles; so that user could add them in carousel.
        Input: No Input
        Output: {'carousel_articles': [{'article_id': int, 'priority': int},
                {'article_id': int, 'priority': int},..], 'published_articles': [{'article_id': int, ….}, {'article_id': int, ….}]}
        """
        response = {}
        query = "SELECT article_id, priority FROM carousel_articles;"
        carousel_articles = QueryHandler.get_results(query)

        query = "SELECT article_id, article_headline, article_image, article_sport_type, to_char(article_publish_date, 'Dy, DD Mon YYYY HH:MI:SS')"\
                " as article_publish_date, article_writer FROM articles WHERE article_state='Published' ORDER BY article_publish_date DESC;"
        published_articles = QueryHandler.get_results(query)
        all_articles = {'carousel': carousel_articles, 'published': published_articles}
        response.update({'status': settings.STATUS_200, 'info': settings.SUCCESS_RESPONSE, 'articles': all_articles})
        self.write(response)


class NewsConsolePostArticlesOnCarousel(BaseRequestHandler):

    def post_carousel_articles(self):
        data = {'articles': self.articles, 'type': 'carousel'}
        requests.post(url=settings.CAROUSEL_ARTICLES_POST_URL, data=json.dumps(data))

    def post(self):
        """
        It enables console user(s) to add article on carousel.
        Input: {'articles': list of published articles, that are to be sent on carousel}
        Output: {'status': 200, 'info': 'Success'} In case of success
                {'status': 400, 'info': 'Bad Info Supplied'} In case no article ids provided
                {'status': 500, 'info': 'Internal Server Error'} In case of any other error
        """
        response = {}
        self.articles = json.loads(self.get_argument('articles'))

        query = "DELETE FROM carousel_articles;"
        QueryHandler.execute(query)

        if self.articles:
            query = "INSERT INTO carousel_articles (article_id, priority) VALUES "
            for key, value in self.articles.items():
                  query += "(%s, %s), " % (int(value), int(key))
            query = query[:-2] + ";"
            QueryHandler.execute(query)
        threading.Thread(group=None, target=self.post_carousel_articles, name=None, args=()).start()
        response.update({'status': settings.STATUS_200, 'info': settings.SUCCESS_RESPONSE})
        self.write(response)


class GetDiscussionsHandler(BaseRequestHandler):
    def get(self):
        response = {}
        response['info'] = Discussion.get_all()
        response['status'] = settings.STATUS_200
        self.write(response)


class JoinDiscussionsHandler(BaseRequestHandler):
    def post(self):
        response = {}
        discussion_id = self.get_argument("discussion_id")
        username = self.get_argument("username")
        info = {"users": [username]}
        Discussion(discussion_id).add_users(info)

        response['group_users'] = get_group_users(discussion_id)
        response['info'] = settings.SUCCESS_RESPONSE
        response['status'] = settings.STATUS_200
        self.write(response)


class PeekDiscussionsHandler(BaseRequestHandler):
    def post(self):
        response = {}
        discussion_id = self.get_argument("discussion_id")
        username = self.get_argument("username")
        Discussion(discussion_id).subscribe_user(username)

        response['group_users'] = get_group_users(discussion_id)
        response['info'] = settings.SUCCESS_RESPONSE
        response['status'] = settings.STATUS_200        
        self.write(response)
