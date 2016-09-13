import ConfigParser
config = ConfigParser.ConfigParser()
config.read('config.py')
from server_component_factory import ServerComponentFactory
from common.funcs import QueryHandler
import settings
import urllib
import json
from node import Node
from s3_object import S3Object
from dp import Dp

class Discussion(Node):
    """
        Handles all the processes relating to a discussion
        A discussion is a group which is created on the curated news content
        Methods are available for :
            1. creating discussions
            2. deleting discussions
            3. Adding users to discussions
            4. Deleting users from discussions
            5. Return all the group discussions happening
    """

    def __init__(self, name, article_id  = None, article_group_name=None):
        self.name = name

        self.article_id = article_id
        self.group_name = article_group_name
        # self.article_images_bucket = config.get('amazon', 'article_images_bucket')
        self.article_images_bucket = "feeds.images"
        # self.article_images_bucket = "curated.article.images"
        self.discussion_admin_password = config.get('server_component', 'discussion_admin_password')
        self.discussion_admin_jid = config.get('server_component', 'discussion_admin_jid')

        self.server_component_factory = ServerComponentFactory(self.discussion_admin_jid, self.discussion_admin_password)

        self.domain = config.get('xmpp', 'domain')

        self.discussion_creation_xml = "<iq to='pubsub.mm.io' type='set' from='" + self.discussion_admin_jid + "'><pubsub xmlns='http://jabber.org/protocol/pubsub'><create node='{}'/><configure><x xmlns='jabber:x:data' type='submit'><field var='pubsub#access_model' type='list-single'><value>open</value></field><field var='pubsub#deliver_payloads' type='boolean'><value>1</value></field><field var='pubsub#notify_retract' type='boolean'><value>1</value></field><field var='pubsub#persist_items' type='boolean'><value>0</value></field><field var='pubsub#presence_based_delivery' type='boolean'><value>0</value></field><field var='pubsub#subscribe' type='boolean'><value>1</value></field><field var='pubsub#publish_model' type='list-single'><value>publishers</value></field><field var='pubsub#title' type='text-single'><value>{}</value></field><field var='pubsub#notification_type' type='text-single'><value>normal</value></field><field var='pubsub#send_last_published_item' type='text-single'><value>never</value></field></x></configure></pubsub></iq> "

        self.affiliate_users_skeleton = "<iq to='pubsub.mm.io' type='set' from='" + self.discussion_admin_jid + "'><pubsub xmlns='http://jabber.org/protocol/pubsub#owner'><affiliations node='{}'>{}</affiliations></pubsub></iq>"
        self.affiliate_user_iterable = "<affiliation jid='{}' affiliation='publisher'/>"

        self.subscribe_user_skeleton = "<iq to='pubsub.mm.io' type='set' from='" + self.discussion_admin_jid + "'><pubsub xmlns='http://jabber.org/protocol/pubsub#owner'><subscriptions node='{}'>{}</subscriptions></pubsub></iq>"
        self.subscribe_user_iterable = "<subscription jid='{}' node='{}' subscription='subscribed'/>"

        self.notification_message = "<message to='{}' from='" + self.discussion_admin_jid + "' type='chat'><body>{}</body><thread>0d34ecb4-47a9-4ffc-afd8-7b064ea7d8be</thread><properties xmlns='http://www.jivesoftware.com/xmlns/xmpp/properties'><property><name>mime_type</name><value type='string'>t</value></property><property><name>time</name><value type='string'>1472631060</value></property></properties></message>"

        self.unsubscribe_user_xml = " <iq to='pubsub.mm.io' from='" + self.discussion_admin_jid + "' type='set'><pubsub xmlns='http://jabber.org/protocol/pubsub'><unsubscribe jid='{}' node='{}'/></pubsub></iq>"
        self.discussion_deletion_xml = "<iq type='set' from='" + self.discussion_admin_jid + "' to='pubsub.mm.io'><pubsub xmlns='http://jabber.org/protocol/pubsub#owner'><delete node='{}'/> </pubsub></iq>"

        self.notify_user_of_user_addition = "<iq to='pubsub.mm.io' from='" + self.discussion_admin_jid + "' type='set'><pubsub xmlns='http://jabber.org/protocol/pubsub'><publish node='{}'><item><message xmlns='pubsub:text:message'>{}</message></item></publish></pubsub></iq>"

        self.article_image_bucket = ""

    def create(self):
        self.server_component_factory.send(self.discussion_creation_xml.format(self.name.strip(), self.group_name))

    def set_dp(self):
        article_image_link = str(self.article_id) + "_xhdpi.png"
        if S3Object(name = article_image_link, bucket_name = self.article_images_bucket).exists():
            content = S3Object(name = article_image_link , bucket_name = self.article_images_bucket).download()
        else:
            content = self.get_random_avatar()
        Dp(self.name).upload_dp(content)

    def add_users(self, info):
        self.server_component_factory.send(self.get_affiliate_users_xml(info["users"]))
        self.server_component_factory.send(self.get_subscribe_users_xml(info["users"]))
        self.server_component_factory.send(self.get_notify_users_xml(info["users"]))

    def get_affiliate_users_xml(self, users):
        affiliate_user_iterable = ""
        for user in users:
            username = user + self.domain

            affiliate_user_iterable += self.affiliate_user_iterable.format(username)

        return self.affiliate_users_skeleton.format(self.name, affiliate_user_iterable)

    def get_subscribe_users_xml(self, users):
        subscribe_users_iterable = ""
        for user in users:
            username = user + self.domain
            subscribe_users_iterable += self.subscribe_user_iterable.format(username, self.name)

        return self.subscribe_user_skeleton.format(self.name, subscribe_users_iterable)

    def get_unsubscribe_user_xml(self, user):
        username = user + self.domain
        return self.unsubscribe_user_xml(username, self.name)

        return self.subscribe_user_skeleton.format(self.name, subscribe_users_iterable)

    def get_notify_users_xml(self, users):
        user_added_notification_message = ""
        for user in users:
            username = user + self.domain
            user_added_notification_payload = urllib.quote(json.dumps({u'group_server_id': unicode(self.name),
                                                                       u'message_from': unicode(self.discussion_admin_jid),
                                                                       u'message_type': unicode(settings.USER_ADDITION_GROUP_MESSAGE_TYPE)
                                                                       }).encode('utf-8'))
            user_added_notification_message += self.notify_user_of_user_addition.format(self.name, user_added_notification_payload)
        return user_added_notification_message

    def create_and_add_users(self, info):
        self.create()
        self.set_dp()
        self.add_users(info)

    def unsubscribe_user(self, user):
        self.server_component_factory.send(self.get_unsubscribe_user_xml(user))

    def delete(self):
        self.server_component_factory.send(self.discussion_deletion_xml.format(self.name))

    def unsubsribe_user_and_delete(self, user):
        self.unsubscribe_user(user)
        self.delete()

    def subscribe_user(self, username):
        self.server_component_factory.send(self.get_subscribe_users_xml([username]))


    @classmethod
    def get_all(cls):
        query = "SELECT articles.article_id AS article_id, articles.article_headline AS headline, articles_discussions.discussion_id AS discussion_id, COUNT(discussions_users.username) AS user_count, to_char(created_at, 'Dy, DD Mon YYYY HH:MI:SS') AS group_creation_date FROM articles, articles_discussions, discussions_users WHERE articles.article_id = articles_discussions.article_id AND articles_discussions.discussion_id = discussions_users.discussion_id GROUP BY articles.article_id, headline, articles_discussions.discussion_id ;"
        return QueryHandler.get_results(query, ())
