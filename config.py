[database]
database=production_db
host=masterdb
user=madmachines
password=password_production_db

# [database]
# database=production_db
# host=localhost
# user=madmachines
# password=password_production_db


[registration]
expiry_period_sec=1800

[pubsub]
pubsub_addr=pubsub.mm.io
pubsub_jid=satish@mm.io
pubsub_password=password
sample_message=Lorem Ipsum is simply dummy text of the printing and typesetting industry please consider donating a small sum to help pay for the hosting and bandwidth bill
node=princely_musings

[amazon]
amazon_access_key=AKIAJQ4YKSYEDJGYH7ZA
amazon_secret_key=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
dp_bucket_name=sports_unity_profile_pics
dp_objects_acl=private
user_media_bucket_name=sports.unity.user.upload
user_media_acl=private
random_avatar_bucket=random.avatars
article_stats_bucket=article_stats
article_images_bucket=article_images
article_ice_breaker_images_bucket=article_ice_breaker_images
article_memes_bucket=article_memes

[xmpp]
domain=@mm.io

[tests]
test_facebook_id=145634995501895
test_phone_number=919560488236
profile_pic_url = http://localhost:3000/profile_pic

[nearby_users]
was_online_limit=7200

[tornado]
listening_port=3000

[apns]
cert_file=priv/prod_apns_cert.pem
key_file=priv/prod_apns_key.pem

[gcm]
api_key=AIzaSyAWO-284HUCY2NrGtf-P5r0o9p4RCe65F0


[google]
url_shortner_key=AIzaSyA1IIZ0kuvDanJBDE6pvMr_X6JeCMs_yiU
