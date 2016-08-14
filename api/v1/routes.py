import handlers
import console_handlers

urls = [
    (r"/v1/set_location_privacy", handlers.LocationPrivacyHandler),
    (r"/v1/set_user_interests", handlers.UserInterestHandler),
    (r"/v1/set_user_watching_match", handlers.SetUserWatchingMatchHandler),
    (r"/v1/delete_user_watching_match", handlers.DeleteUserWatchingMatchHandler),
    (r"/v1/friends_watching", handlers.GetUserWatchingMatchHandler),

    ## console urls
    (r"/news_login", console_handlers.NewsConsoleLogin),
    (r"/news_add_user", console_handlers.NewsConsoleAddUser),
    (r"/news_upload_s3_object", console_handlers.NewsConsoleUploadS3Object),
    (r"/add_article", console_handlers.NewsConsoleAddCuratedArticle),
    (r"/fetch_articles", console_handlers.NewsConsoleFetchArticles),
    (r"/edit_article", console_handlers.NewsConsoleEditArticle),
    (r"/delete_article", console_handlers.NewsConsoleDeleteArticle),
    (r"/publish_article", console_handlers.NewsConsolePublishArticle),
    (r"/post_carousel_articles", console_handlers.NewsConsolePostArticlesOnCarousel)
]
