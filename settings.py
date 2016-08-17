
TORNADO_LOCALHOST = "localhost"
TORANDO_PORT = 3000

STATUS_500 = 500
STATUS_400 = 400
STATUS_200 = 200
STATUS_404 = 404
STATUS_422 = 422
STATUS_403 = 403
STATUS_409 = 409
STATUS_405 = 405
STATUS_ERROR_LIST = [STATUS_400, STATUS_404, STATUS_500, STATUS_422, STATUS_403]

SUCCESS_RESPONSE = "Success"
MISSING_APK_AND_UDID_ERROR = "Bad Request: Please provide 'apk_version' and 'udid'"
BAD_AUTHENTICATION_ERROR = "Bad Authentication Info"
USER_FORBIDDEN_ERROR = "Forbidden: The user is BLOCKED!"
BAD_INFO_ERROR = " Bad Info Supplied for {}"
DUPLICATE_KEY_ERROR = "Database Error: This {} already exists"
INVALID_BUCKET_ERROR = "Bad Bucket Request"
KEY_ALREADY_EXISTS = "Bad Request: This Key already exists in the bucket"
INTERNAL_SERVER_ERROR = "Internal server error"

# admin templates path
ADMIN_TEMPLATES_PATH = 'admin_templates/'
ADMIN_TEMPLATES = [
    'admin.html',
    'select_users.html',
    'create_user.html',
    'update_user.html',
    'delete_user.html',
    'block_unblock_user.html']

# App testing
TESTING_NUMBER_1 = '918989898989'
TESTING_NUMBER_2 = '911010101010'
TESTING_NUMBER_3 = '911212121212'

APP_TESTING_PHONE_NUMBERS = [
    TESTING_NUMBER_1,
    TESTING_NUMBER_2,
    TESTING_NUMBER_3
]

APP_TESTING_OTP = {
    TESTING_NUMBER_1: 1234,
    TESTING_NUMBER_2: 4567,
    TESTING_NUMBER_3: 7890
}

# SINFINI message gateway
SINFINI_MESSAGE_GATEWAY = 'http://global.sinfini.com/api/v3/index.php'
SINFINI_API_KEY = 'A8e2b6ece6237e5ebc6a0631e51e4ec43'
SINFINI_SENDER_ID = 'SPORTU'

OTP_MESSAGE = 'Welcome to Sports Unity. Your authorization code is {}'
APP_INVITATION_MESSAGE = 'Thank you for reaching out \n Android users can download the app at http://sportsunity.co/sports \n IOS users we are soon coming in the app store too \n Thank you'


TOKEN_ANDROID_TYPE = 'a'
TOKEN_IOS_TYPE = 'i'
DP_S_WIDTH = 150
DP_S_HEIGHT = 150
DP_L_WIDTH = 640
DP_L_HEIGHT = 640

SHOW_LOCATION_FRIENDS_STATUS = 'f'
SHOW_LOCATION_ALL_STATUS = 'a'
SHOW_LOCATION_NONE_STATUS = 'n'

PLAY_STORE_URL="https://play.google.com/store/apps/details?id=co.sports.unity&referrer=utm_source%3D{}%26utm_medium%3Dsp-andr"
API_SHORTNER_ENDPOINT="https://www.googleapis.com/urlshortener/v1/url?key={}"

RANDOM_PICTURE_COUNT = 1000

USER_TYPE_HUMAN = 'u'
USER_TYPE_SERVER_BOT = 's'

ARTICLE_POLL_ANSWER_TYPES = ['y', 'n']
PUBSUB_COMMENTARY__NODE_NAME_SUFFIX = "match_"

# curated articles' buckets' mapping
articles_BUCKETS = {
    'stats': 'curated.article.stats',
    'news_image': 'curated.article.images',
    'ice_breaker_image': 'curated.article.icebreaker.images',
    'memes': 'curated.article.memes'
}

NEWS_SERVER = 'http://NewsLB-388179569.ap-northeast-2.elb.amazonaws.com'
articles_POST_URL = NEWS_SERVER + '/articles'
