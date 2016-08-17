import ConfigParser
config = ConfigParser.ConfigParser()
config.read('config.py')
from server_component_factory import ServerComponentFactory
class Discussion(object):

	discussion_admin_password = config.get('server_component', 'discussion_admin_password') 
	discussion_admin_jid = config.get('server_component', 'discussion_admin_jid') 
	server_component = ServerComponentFactory().get_server_component(discussion_admin_jid, discussion_admin_password)

	domain = config.get('xmpp', 'domain') 

	discussion_creation_xml = "<iq to='pubsub.mm.io' type='set' from='admin@mm.io'><pubsub xmlns='http://jabber.org/protocol/pubsub'><create node='{}'/><configure><x xmlns='jabber:x:data' type='submit'><field var='pubsub#access_model' type='list-single'><value>whitelist</value></field><field var='pubsub#deliver_payloads' type='boolean'><value>1</value></field><field var='pubsub#notify_retract' type='boolean'><value>1</value></field><field var='pubsub#persist_items' type='boolean'><value>0</value></field><field var='pubsub#presence_based_delivery' type='boolean'><value>0</value></field><field var='pubsub#subscribe' type='boolean'><value>1</value></field><field var='pubsub#publish_model' type='list-single'><value>publishers</value></field><field var='pubsub#title' type='text-single'><value>testgroup</value></field><field var='pubsub#notification_type' type='text-single'><value>normal</value></field><field var='pubsub#send_last_published_item' type='text-single'><value>never</value></field></x></configure></pubsub></iq> "
	add_users_to_discussion_xml = "<iq to='pubsub.mm.io' type='set' from='admin@mm.io'><pubsub xmlns='http://jabber.org/protocol/pubsub#owner'><subscriptions node='{}'><subscription jid='{}' node='{}' subscription='subscribed'/></subscriptions></pubsub></iq>"
	unsubsribe_user_from_discussion_xml = "<iq to='pubsub.mm.io' id='HQAH3-42' type='set' from='admin@mm.io'><pubsub xmlns='http://jabber.org/protocol/pubsub#owner'><subscriptions node='{}'><subscription jid='{}' node='{}' subscription='none'/></subscriptions></pubsub></iq>"
	discussion_deletion_xml = "<iq type='set' from='admin@mm.io' to='pubsub.mm.io' id='delete1'><pubsub xmlns='http://jabber.org/protocol/pubsub#owner'><delete node='{}'/> </pubsub></iq>"

	def __init__(self, name):
		self.name = name

	def create(self):
		self.server_component.send_raw(self.discussion_creation_xml.format(self.name.strip()))


	def add_users(self, info):
		for user in info["users"]:
			username = user + self.domain
			self.server_component.send_raw(self.add_users_to_discussion_xml.format(self.name, username, self.name))

	def create_and_add_users(self, info):
		self.create()
		self.add_users(info)

	def unsubscribe_user(self, username):
		self.server_component.send_raw(self.unsubsribe_user_from_discussion_xml.format(self.name, username, self.name))

	def delete(self):
		self.server_component.send_raw(self.discussion_deletion_xml.format(self.name))

	def unsubsribe_user_and_delete(self, username):
		self.unsubscribe_user(username)
		self.delete()