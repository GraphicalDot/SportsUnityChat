"""
Add curated articles' tables
"""

from yoyo import step

__depends__ = {}

steps = [
    step("CREATE TABLE content_writers (writer_id serial PRIMARY KEY, username TEXT UNIQUE NOT NULL, "
         "password TEXT NOT NULL, role TEXT CONSTRAINT check_writer_role CHECK (role IN ('admin', 'author')), "
         "created_at TIMESTAMP DEFAULT now());"),

    step("CREATE TABLE articles (article_id serial PRIMARY KEY, article_headline TEXT NOT NULL, "
         "article_content TEXT NOT NULL, article_image TEXT, article_poll_question TEXT NOT NULL, "
         "article_ice_breaker_image TEXT, article_sport_type TEXT CONSTRAINT check_sport_type CHECK (article_sport_type IN ('c', 'f')), "
         "article_publish_date TIMESTAMP NOT NULL DEFAULT now(), article_stats TEXT[], article_memes TEXT[], article_state TEXT NOT NULL, created_at TIMESTAMP DEFAULT now());"),

    step("CREATE TABLE memes (meme_id serial PRIMARY KEY, meme_image TEXT NOT NULL, created_at TIMESTAMP DEFAULT now());"),

    step("CREATE TABLE tags (tag_id serial PRIMARY KEY, tag_name TEXT UNIQUE NOT NULL, created_at TIMESTAMP DEFAULT now());"),

    step("CREATE TABLE memes_tags (meme_id INTEGER REFERENCES memes(meme_id) ON UPDATE CASCADE ON DELETE CASCADE, "
         "tag_id INTEGER REFERENCES tags(tag_id) ON UPDATE CASCADE ON DELETE CASCADE, created_at TIMESTAMP DEFAULT now());"),

    step("CREATE UNIQUE INDEX i_memes_tags_id ON memes_tags USING btree (meme_id, tag_id);"),

    step("CREATE TABLE carousel_articles (id serial PRIMARY KEY, article_id INTEGER REFERENCES articles (article_id) ON DELETE CASCADE, "
         "priority INTEGER UNIQUE);")

]
