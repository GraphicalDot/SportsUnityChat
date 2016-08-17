import ConfigParser
config = ConfigParser.ConfigParser()
config.read('config.py')
from s3_image import S3Image
from s3_object import S3Object
from node import Node
from server_component_factory import ServerComponentFactory
class Group(Node):
	def __init__(self, name):
		self.name = name
		self.dp_bucket = str.strip(config.get('amazon', 'dp_bucket_name'))

	def upload_dp(self, content):
		image = S3Image(self.name, self.dp_bucket, content)
		image.version()
		image.handle_upload()

	def get_dp_version(self, version):
		if not version in ["L", "S"]:
			raise BadInfoSuppliedError("photo version")
		key = self.name + "/" + version + ".jpg"
		return S3Object(name = self.jid, bucket_name = self.dp_bucket).download()