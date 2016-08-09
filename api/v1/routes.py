import handlers
import news_console_api as news_handlers

urls = [
    (r"/v1/set_location_privacy", handlers.LocationPrivacyHandler),
    (r"/v1/set_user_interests", handlers.UserInterestHandler),
    (r"/v1/set_user_watching_match", handlers.SetUserWatchingMatchHandler),
    (r"/v1/delete_user_watching_match", handlers.DeleteUserWatchingMatchHandler),
    (r"/v1/friends_watching", handlers.GetUserWatchingMatchHandler),

    ## console urls
    (r"/news_login", news_handlers.NewsConsoleLogin),
    (r"/news_add_user", news_handlers.NewsConsoleAddUser),
    (r"/news_upload_s3_object", news_handlers.NewsConsoleUploadS3Object),
    (r"/add_article", news_handlers.NewsConsoleAddCuratedArticle),
    (r"/fetch_articles", news_handlers.NewsConsoleFetchArticles),
    (r"/edit_article", news_handlers.NewsConsoleEditArticle),
]
