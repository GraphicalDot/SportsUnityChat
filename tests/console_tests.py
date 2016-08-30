import sys
import os

import tornado
from dateutil import parser

sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import base64
import boto3
import json
import requests
import settings
import test_utils
import unittest
import random

from ConfigParser import ConfigParser
from wand.color import Color
from wand.image import Image as wImage

from common.funcs import QueryHandler

config = ConfigParser()
config.read('config.py')

TORNADO_PORT =  int(config.get('tornado', 'listening_port'))
TORNADO_SERVER =  "http://localhost:%u" % TORNADO_PORT

amazon_access_key = str.strip(str(config.get('amazon', 'amazon_access_key')))
amazon_secret_key = str.strip(str(config.get('amazon', 'amazon_secret_key')))


class NewsConsoleLoginTests(unittest.TestCase):

    def setUp(self):
        self.login_url = TORNADO_SERVER +  '/news_login'
        self.data = dict()
        test_utils.delete_field_from_table('content_writers', 'username', 'test_user_1')
        test_utils.register_content_writer(username='test_user_1', password='test_user_1', role='admin')

    def test_get(self):
        response = requests.get(self.login_url)
        self.assertEqual(response.status_code, settings.STATUS_405)

    def test_post(self):

        # case: no POST arguments provided
        response = requests.post(url=self.login_url)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_400)
        self.assertEqual(res['info'], "Missing argument username")

        # case: 'password' not provided
        self.data = {'username': 'test_user_2'}
        response = requests.post(self.login_url, self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_400)
        self.assertEqual(res['info'], "Missing argument password")

        # case: user not registered
        self.data['password'] = 'test_user_2'
        response = requests.post(self.login_url, self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_404)
        self.assertEqual(res['info'], settings.BAD_AUTHENTICATION_ERROR)

        # case: user already registered
        self.data = {'username': 'test_user_1', 'password': 'test_user_1'}
        response = requests.post(self.login_url, self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        self.assertEqual(res['user_role'], 'admin')

    def tearDown(self):
        test_utils.delete_user_from_table(field='username', table='content_writers', field_value='test_user_1')


class NewsConsoleAddUserTests(unittest.TestCase):

    def setUp(self):
        self.add_user_url = TORNADO_SERVER + '/news_add_user'
        self.data = dict()
        test_utils.delete_field_from_table('content_writers', 'username', 'test_user_1')
        test_utils.register_content_writer(username='test_user_1', password='test_user_1', role='admin')

    def test_get(self):
        response = requests.get(self.add_user_url)
        self.assertEqual(response.status_code, settings.STATUS_405)

    def test_post(self):
        # case: user already registered
        self.data = {'username': 'test_user_1', 'password': 'test_user_1', 'user_role': 'admin'}
        response = requests.post(url=self.add_user_url, data=self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_409)
        self.assertEqual(res['info'], settings.DUPLICATE_KEY_ERROR.format('username'))

        # case: new user registered with wrong role type
        self.data = {'username': 'test_user_2', 'password': 'test_user_2', 'user_role': 'writer'}
        response = requests.post(url=self.add_user_url, data=self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_400)
        self.assertEqual(res['info'], settings.INTERNAL_SERVER_ERROR)

        # case: new user registered with correct role type
        self.data['user_role'] = 'author'
        response = requests.post(url=self.add_user_url, data=self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        query = "SELECT username FROM content_writers WHERE username = '%s';" % (self.data['username'],)
        self.assertEqual(len(QueryHandler.get_results(query)), 1)

    def tearDown(self):
        test_utils.delete_user_from_table(field='username', table='content_writers', field_value='test_user_1')
        test_utils.delete_user_from_table(field='username', table='content_writers', field_value='test_user_2')


class NewsConsoleUploadS3ObjectTests(unittest.TestCase):

    def setUp(self):
        self.upload_object_url = TORNADO_SERVER + '/news_upload_s3_object'
        self.data = dict()
        self.bucket_name = None
        self.s3_client = boto3.client('s3', aws_access_key_id = amazon_access_key, aws_secret_access_key = amazon_secret_key)
        self.content = base64.b64encode(wImage(width=640, height=640, background=Color('blue')).make_blob(format='png'))

    def test_post(self):

        # incomplete post data
        self.data = {'name': 'test_image_blue', 'type': 'news_image'}
        response = requests.post(url=self.upload_object_url, data=self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_400)
        self.assertEqual(res['info'], 'Missing argument content')

        # invalid object type
        self.data['content'] = self.content
        self.data['type'] = 'invalid'
        response = requests.post(url=self.upload_object_url, data=self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_400)
        self.assertEqual(res['info'], settings.INVALID_BUCKET_ERROR)

        # new object upload
        self.data['type'] = 'news_image'
        self.bucket_name = settings.articles_BUCKETS.get(self.data['type'])
        response = requests.post(url=self.upload_object_url, data=self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        self.assertEqual(res['link'], "{}/{}/{}".format(self.s3_client.meta.endpoint_url, self.bucket_name, base64.encodestring(self.data['name'])))
        self.s3_client.get_object(Bucket = self.bucket_name, Key = base64.encodestring(self.data['name']))

        # object already exists
        response = requests.post(url=self.upload_object_url, data=self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_409)
        self.assertEqual(res['info'], settings.KEY_ALREADY_EXISTS)

    def tearDown(self):
        if self.bucket_name:
            self.s3_client.delete_object(Bucket = self.bucket_name, Key = base64.encodestring(self.data['name']))


class NewsConsoleAddCuratedArticleTests(unittest.TestCase):

    def setUp(self):
        self.data = dict()
        self.article_id = None
        self.add_curated_article_url = TORNADO_SERVER + '/add_article'
        self.news_image_bucket = settings.articles_BUCKETS.get('news_image')
        self.ice_breaker_bucket = settings.articles_BUCKETS.get('ice_breaker_image')
        self.s3_client = boto3.client('s3', aws_access_key_id = amazon_access_key, aws_secret_access_key = amazon_secret_key)
        test_utils.register_content_writer('test_user', 'test_user', 'author')

    def test_post(self):

        # incomplete required POST data
        self.data = {'username': 'test_user', 'article_headline': 'TEST_HEADLINE'}
        response = requests.post(url=self.add_curated_article_url, data=self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_400)
        self.assertEqual(res['info'], 'Missing argument article_content')

        # valid POST data
        self.data.update({'article_content': 'TEST_CONTENT', 'article_poll_question': 'TEST_POLL_QUESTION',
                          'article_notification_content': 'TEST_HEADLINE', 'article_sport_type': 'c', 'article_stats': ['link_1', 'link_2'],
                          'article_memes': ['meme_1'], 'article_state': 'UnPublished'})

        image_path = os.getcwd() + '/tests/test_image.jpeg'
        self.files = {'article_image': open(image_path, 'r').read(), 'article_ice_breaker_image': open(image_path, 'r').read()}
        response = requests.post(url=self.add_curated_article_url, data=self.data, files=self.files)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        query = "SELECT article_id FROM articles WHERE article_headline = '%s';" % (self.data['article_headline'],)
        result = QueryHandler.get_results(query)
        self.assertEqual(len(result), 1)
        self.article_id = str(result[0]['article_id'])
        self.s3_client.get_object(Bucket = self.news_image_bucket, Key = str(self.article_id) + '.jpeg')
        self.s3_client.get_object(Bucket = self.ice_breaker_bucket, Key = str(self.article_id) + '.jpeg')

    def tearDown(self):
        test_utils.delete_field_from_table('articles', 'article_headline', 'TEST_HEADLINE')
        test_utils.delete_field_from_table('content_writers', 'username', 'test_user')
        self.s3_client.delete_object(Bucket=self.news_image_bucket, Key=str(self.article_id) + '.jpeg')
        self.s3_client.delete_object(Bucket=self.ice_breaker_bucket, Key=str(self.article_id) + '.jpeg')


class NewsConsoleFetchArticlesTests(unittest.TestCase):

    def setUp(self):
        self.fetch_articles_url = TORNADO_SERVER + '/fetch_articles'
        self.data = dict()
        test_utils.delete_field_from_table('content_writers', 'username', 'test_user_1')
        test_utils.register_content_writer('test_user_1', 'test_user_1', 'admin')
        test_utils.register_content_writer('test_user_2', 'test_user_2', 'author')
        test_utils.register_content_writer('test_user_3', 'test_user_3', 'author')
        self.articles = [{'headline': 'article_1', 'sport_type': 'c', 'state': 'UnPublished', 'writer': 'test_user_1'},
                         {'headline': 'article_2', 'sport_type': 'f', 'state': 'Draft', 'writer': 'test_user_1'},
                         {'headline': 'article_3', 'sport_type': 'f', 'state': 'Draft', 'writer': 'test_user_2'},
                         {'headline': 'article_4', 'sport_type': 'c', 'state': 'Draft', 'writer': 'test_user_3'},
                         {'headline': 'article_5', 'sport_type': 'c', 'state': 'Published', 'writer': 'test_user_3'}]
        self.article_ids = test_utils.create_articles(self.articles)

    def test_post(self):
        response = requests.post(self.fetch_articles_url)
        self.assertEqual(response.status_code, settings.STATUS_405)

    def test_get(self):

        # user is admin
        self.data = {'username': 'test_user_1'}
        response = requests.get(self.fetch_articles_url, self.data)
        res = json.loads(response.text)
        articles_result = json.loads(res['articles'])
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        self.assertEqual(len(articles_result), 3)
        self.assertEqual([article['article_headline'] for article in articles_result], ['article_5', 'article_2', 'article_1'])

        # user is not admin
        self.data = {'username': 'test_user_3'}
        response = requests.get(self.fetch_articles_url, self.data)
        res = json.loads(response.text)
        articles_result = json.loads(res['articles'])
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        self.assertEqual(len(articles_result), 2)
        self.assertEqual([article['article_headline'] for article in articles_result], ['article_5', 'article_4'])

        # user-admin, article_sport_type-cricket, article_state-Drafts
        self.data = {'username': 'test_user_1', 'article_sport_type': 'f', 'article_state': 'Draft'}
        response = requests.get(self.fetch_articles_url, self.data)
        res = json.loads(response.text)
        articles_result = json.loads(res['articles'])
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        self.assertEqual(len(articles_result), 1)
        self.assertEqual(articles_result[0]['article_headline'], 'article_2')

        # user-admin, article_sport_type-cricket, article_state-UnPublished
        self.data.update({'article_sport_type': 'c', 'article_state': 'UnPublished'})
        response = requests.get(self.fetch_articles_url, self.data)
        res = json.loads(response.text)
        articles_result = json.loads(res['articles'])
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        self.assertEqual(len(articles_result), 1)
        self.assertEqual(articles_result[0]['article_headline'], 'article_1')

        # user-author, article_sport_type-football
        self.data = {'username': 'test_user_2', 'article_sport_type': 'f'}
        response = requests.get(self.fetch_articles_url, self.data)
        res = json.loads(response.text)
        articles_result = json.loads(res['articles'])
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        self.assertEqual(len(articles_result), 1)
        self.assertEqual(articles_result[0]['article_headline'], 'article_3')

        # user-author, article_sport_type-cricket, article_state-UnPublished
        self.data.update({'username': 'test_user_3', 'article_sport_type': 'c', 'article_state': 'Published'})
        response = requests.get(self.fetch_articles_url, self.data)
        res = json.loads(response.text)
        articles_result = json.loads(res['articles'])
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        self.assertEqual(len(articles_result), 1)
        self.assertEqual(articles_result[0]['article_headline'], 'article_5')

        # wrong article_sport_type provided
        self.data.update({'article_sport_type': 'w'})
        response = requests.get(self.fetch_articles_url, self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_400)
        self.assertEqual(res['info'], settings.BAD_INFO_ERROR.format('article_sport_type'))

    def tearDown(self):
        test_utils.delete_field_from_table('content_writers', 'username', 'test_user_1')
        test_utils.delete_field_from_table('content_writers', 'username', 'test_user_2')
        test_utils.delete_field_from_table('content_writers', 'username', 'test_user_3')
        test_utils.delete_articles(self.article_ids)


class NewsConsoleGetArticleTests(unittest.TestCase):

    def setUp(self):
        self.get_article_url = TORNADO_SERVER + '/get_article'
        test_utils.delete_field_from_table('content_writers', 'username', 'test_user_1')
        test_utils.register_content_writer('test_user_1', 'test_user_1', 'author')
        self.articles = [{'headline': 'article_1', 'sport_type': 'c', 'state': 'UnPublished', 'writer': 'test_user_1'},
                         {'headline': 'article_2', 'sport_type': 'f', 'state': 'Published', 'writer': 'test_user_1'}]
        self.article_ids = test_utils.create_articles(self.articles)

    def test_get(self):

        # invalid 'article_id' provided
        response = requests.get(self.get_article_url + '?article_id=100000')
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_400)
        self.assertEqual(res['info'], settings.BAD_INFO_ERROR.format('article_id'))

        # valid 'aricle_id' provided
        response = requests.get(self.get_article_url + '?article_id=%s' % str(self.article_ids[1]))
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        self.assertEqual(res['article']['article_headline'], 'article_2')

    def tearDown(self):
        test_utils.delete_articles(self.article_ids)


class NewsConsoleEditArticleTests(unittest.TestCase):

    def setUp(self):
        self.edit_article_url = TORNADO_SERVER + '/edit_article'
        test_utils.register_content_writer('test_user', 'test_user', 'author')
        self.articles = [{'headline': 'article_1', 'sport_type': 'c', 'state': 'UnPublished', 'writer': 'test_user'},
                         {'headline': 'article_2', 'sport_type': 'f', 'state': 'Published', 'writer': 'test_user'}]
        self.article_ids = test_utils.create_articles(self.articles)

    def test_post(self):

        # invalid argument key provided
        self.data = {'article_id': self.article_ids[0], 'invalid_key': 'text'}
        response = requests.post(self.edit_article_url, self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_400)
        self.assertEqual(res['info'], settings.INVALID_KEY_ERROR)

        # valid 'article_id'
        self.data = {'article_id': self.article_ids[1], 'article_content': 'CHANGED_TEXT', 'article_stats': ['STAT_LINK_1', 'STAT_LINK_2']}
        response = requests.post(self.edit_article_url, self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        query = "SELECT article_content FROM articles WHERE article_id = %s;"
        variables = (self.data['article_id'],)
        result = QueryHandler.get_results(query, variables)
        self.assertEqual(len(result), 1)
        self.assertEqual(result[0]['article_content'], self.data['article_content'])

    def tearDown(self):
        test_utils.delete_field_from_table('content_writers', 'username', 'test_user')
        test_utils.delete_articles(self.article_ids)


class NewsConsoleDeleteArticleTests(unittest.TestCase):

    def create_carousel_article(self):
        query = "INSERT INTO carousel_articles (article_id, priority) VALUES (%s, %s);"
        variables = (self.article_ids[0], 1)
        QueryHandler.execute(query, variables)

    def setUp(self):
        self.delete_article_url = TORNADO_SERVER + '/delete_article'
        self.data = dict()
        test_utils.register_content_writer('test_user', 'test_user', 'author')
        self.articles = [{'headline': 'article_1', 'sport_type': 'c', 'state': 'Published', 'writer': 'test_user'},
                         {'headline': 'article_2', 'sport_type': 'f', 'state': 'UnPublished', 'writer': 'test_user'}]
        self.article_ids = test_utils.create_articles(self.articles)
        self.create_carousel_article()

    def test_get(self):
        self.data = {'article_id': self.article_ids[0]}
        response = requests.get(self.delete_article_url, self.data)
        self.assertEqual(response.status_code, settings.STATUS_405)

    def test_post(self):

        # article_id not provided
        self.data = {}
        response = requests.post(self.delete_article_url, self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_400)
        self.assertEqual(res['info'], 'Missing argument article_id')

        # valid article_id and force_delete = False provided; article to be deleted is in carousel
        self.data.update({'article_id': self.article_ids[0], 'force_delete': 0})
        response = requests.post(self.delete_article_url, self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        self.assertEqual(res['in_carousel'], True)

        # valid article_id and force_delete = False provided; article to be deleted is not in carousel
        self.data.update({'article_id': self.article_ids[1]})
        response = requests.post(self.delete_article_url, self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        self.assertEqual(res['in_carousel'], False)
        query = "SELECT article_headline FROM articles WHERE article_id = %s;" % self.article_ids[1]
        result = QueryHandler.get_results(query)
        self.assertEqual(len(result), 0)

        # valid article_id and force_delete = True provided
        self.data.update({'article_id': self.article_ids[0], 'force_delete': 1})
        response = requests.post(self.delete_article_url, self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        query = "SELECT article_headline FROM articles WHERE article_id = %s;" % self.article_ids[0]
        result = QueryHandler.get_results(query)
        self.assertEqual(len(result), 0)

        query = "SELECT article_id FROM carousel_articles WHERE article_id = %s;" % self.article_ids[0]
        result = QueryHandler.get_results(query)
        self.assertEqual(len(result), 0)

    def tearDown(self):
        test_utils.delete_field_from_table('content_writers', 'username', 'test_user')
        test_utils.delete_articles(self.article_ids)


class NewsConsolePublishArticleTests(unittest.TestCase):

    def setUp(self):
        self.publish_article_url = TORNADO_SERVER + '/publish_article'
        test_utils.register_content_writer('test_user', 'test_user', 'author')
        self.articles = [{'headline': 'article_1', 'sport_type': 'c', 'state': 'UnPublished', 'writer': 'test_user'},
                         {'headline': 'article_2', 'sport_type': 'c', 'state': 'UnPublished', 'writer': 'test_user'}]
        self.article_ids = test_utils.create_articles(self.articles)

    def test_post(self):

        # invalid 'article_id' provided
        response = requests.post(self.publish_article_url, {'article_id': self.article_ids[0] + 2000})
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_400)
        self.assertEqual(res['info'], settings.BAD_INFO_ERROR.format('article_id'))

        # valid 'article_id' provided
        response = requests.post(self.publish_article_url, {'article_id': self.article_ids[0]})
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        self.assertEqual(res['article']['article_state'], 'Published')

    def tearDown(self):
        test_utils.delete_field_from_table('content_writers', 'username', 'test_user')
        test_utils.delete_articles(self.article_ids)


class NewsConsolePostArticlesOnCarouselTests(unittest.TestCase):

    def setUp(self):
        self.post_carousel_article_url = TORNADO_SERVER + '/post_carousel_articles'
        test_utils.register_content_writer('test_user', 'test_user', 'author')
        self.articles = [{'headline': 'article_1', 'sport_type': 'c', 'state': 'Published', 'writer': 'test_user'},
                         {'headline': 'article_2', 'sport_type': 'f', 'state': 'Published', 'writer': 'test_user'}]
        self.article_ids = test_utils.create_articles(self.articles)
        self.s3_client = boto3.client('s3', aws_access_key_id = amazon_access_key, aws_secret_access_key = amazon_secret_key)

    def test_get(self):
        response = requests.get(self.post_carousel_article_url)
        self.assertEqual(response.status_code, settings.STATUS_405)

    def test_post(self):
        # article_ids not provided
        response = requests.post(self.post_carousel_article_url)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_400)
        self.assertEqual(res['info'], 'Missing argument articles')

        # valid article_ids provided
        response = requests.post(self.post_carousel_article_url,
                                 {'articles': json.dumps({'100': self.article_ids[0], '101': self.article_ids[1]})})
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        query = "SELECT article_id FROM carousel_articles WHERE priority = 100;"
        result = QueryHandler.get_results(query)
        self.assertEqual(result[0]['article_id'], self.article_ids[0])

    def tearDown(self):
        test_utils.delete_field_from_table('content_writers', 'username', 'test_user')
        test_utils.delete_articles(self.article_ids)


class NewsConsoleGetCarouselArticlesTests(unittest.TestCase):

    def create_carousel_articles(self):
        query = "INSERT INTO carousel_articles (article_id, priority) VALUES (%s, %s), (%s, %s);"
        variables = (self.article_ids[0], 1, self.article_ids[3], 2)
        QueryHandler.execute(query, variables)

    def setUp(self):
        self.get_carousel_articles_url = TORNADO_SERVER + '/get_carousel_articles'
        test_utils.register_content_writer('test_user', 'test_user', 'author')
        self.articles = [{'headline': 'article_1', 'sport_type': 'c', 'state': 'Published', 'writer': 'test_user'},
                         {'headline': 'article_2', 'sport_type': 'f', 'state': 'UnPublished', 'writer': 'test_user'},
                         {'headline': 'article_3', 'sport_type': 'f', 'state': 'Published', 'writer': 'test_user'},
                         {'headline': 'article_4', 'sport_type': 'c', 'state': 'Published', 'writer': 'test_user'},
                         {'headline': 'article_5', 'sport_type': 'c', 'state': 'Published', 'writer': 'test_user'}]
        self.article_ids = test_utils.create_articles(self.articles)
        self.create_carousel_articles()

    def test_get(self):
        response = requests.get(self.get_carousel_articles_url)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        self.assertEqual(len(res['articles']['carousel']), 2)
        self.assertEqual(len(res['articles']['published']), 4)

    def tearDown(self):
        test_utils.delete_field_from_table('content_writers', 'username', 'test_user')
        test_utils.delete_articles(self.article_ids)


class GetDiscussionsHandlerTest(unittest.TestCase):
    _get_all_discussions_url = TORNADO_SERVER + "/get_all_discussions"

    _user_count = 10
    _users = []

    def allocate_groups(self, article_id, username, poll_answer):
        query = " SELECT * FROM assign_discussion(%s, %s, %s);"
        variables = (article_id, username, poll_answer)
        result = QueryHandler.get_results(query, variables)

    def get_discussion_id(self, user_info):
        query = " SELECT discussion_id FROM discussions_users WHERE discussions_users.username = %s ;"
        variables = (user_info["username"], )
        return QueryHandler.get_results(query, variables)


    def setUp(self):
        query = " INSERT INTO articles (article_headline, article_content, article_poll_question, article_ice_breaker_image) VALUES ('test', 'test', 'test', 'test') RETURNING article_id;"
        self._article_id = QueryHandler.get_results(query, ())[0]['article_id']

        for index in range(0, self._user_count):
            username = "test_user" + str(index)
            password = "test_password" + str(index)
            phone_number = "test_ph" + str(index)
            poll_answer = 'y' if random.randint(0,1) == 1 else 'n'

            test_utils.delete_user(username = username)
            test_utils.create_user(username = username, password = password, phone_number = phone_number)

            self.allocate_groups(self._article_id, username, poll_answer)

            user_info = {"username": username, "password": password , "phone_number": phone_number, "poll_answer": poll_answer}
            self._users.append(user_info)

    def test_get(self):
        response = json.loads(requests.get(self._get_all_discussions_url).content)
        assert response['status'] == settings.STATUS_200

        assert response['info'][0]['article_id'] == self._article_id
        assert response['info'][0]['headline']
        assert response['info'][0]['discussion_id']
        assert response['info'][0]['user_count']


    def tearDown(self):
        for user in self._users:
            test_utils.delete_user(username = user["username"])

        query = "DELETE FROM articles WHERE article_id = %s;"
        variables = (self._article_id,)
        QueryHandler.execute(query, variables)


if __name__ == '__main__':
    unittest.main()
