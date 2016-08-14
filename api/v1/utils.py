import settings
import threading

from common.funcs import S3
from common.custom_error import InvalidBucketRequest, KeyAlreadyExists
from models.s3_object import S3Object


class ConsoleS3Object(S3):

    def __init__(self, object_type, image_name, content, acl='private'):
        self.bucket_name = self.get_bucket_name(object_type)
        super(ConsoleS3Object, self).__init__(bucket_name = self.bucket_name, name = image_name, content = content, acl = acl)

    def get_bucket_name(self, object_type):
        bucket_name = settings.CURATED_ARTICLES_BUCKETS.get(object_type, None)
        if bucket_name:
            return bucket_name
        else:
            raise InvalidBucketRequest

    def handle_upload(self):
        if self.check_exists():
            raise KeyAlreadyExists
        else:
		    threading.Thread(group = None, target = self.upload, name = None, args = ()).start()

    def upload(self):
        self.client.put_object(Bucket = self.bucket_name, Key = self.name, Body=self.content, ACL = self.acl)
