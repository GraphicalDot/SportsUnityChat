import settings
from models.s3_object import S3Object


def upload_s3_object(name, content, object_type):
    print 'inside upload_s3_object'
    response = {}
    s3_bucket_name = settings.CURATED_ARTICLES_BUCKETS.get(object_type, None)
    if s3_bucket_name:
        s3_object = S3Object(bucket_name=s3_bucket_name, name=name, content=content, acl='public-read')
        if s3_object.exists():
            response.update({'status': settings.STATUS_409, 'info': settings.OBJECT_ALREADY_EXISTS.format(object_type)})
        else:
            s3_object.upload()
            s3_object_link = "{}/{}/{}".format(s3_object.client.meta.endpoint_url, s3_bucket_name, name)
            response.update({'status': settings.STATUS_200, 'info': settings.SUCCESS_RESPONSE, 'link': s3_object_link})
    else:
        response.update({'status': settings.STATUS_404, 'info': settings.INVALID_OBJECT_TYPE})
    return response
