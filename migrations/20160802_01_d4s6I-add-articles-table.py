"""

"""

from yoyo import step

__depends__ = {}

steps = [
    step("CREATE TABLE articles (article_id SERIAL PRIMARY KEY, article_headline TEXT NOT NULL, article_text TEXT NOT NULL, article_image TEXT, article_poll_question TEXT NOT NULL, article_ice_breaker_image TEXT NOT NULL, stats TEXT ARRAY, created_at TIMESTAMP DEFAULT now());"),
    step("CREATE TABLE memes (meme_id SERIAL PRIMARY KEY, created_at TIMESTAMP DEFAULT now());"),
    step("CREATE TABLE image_tags (image_tag_id SERIAL PRIMARY KEY, image_tag TEXT NOT NULL, created_at TIMESTAMP DEFAULT now());"),
    step("CREATE TABLE memes_image_tags (meme_id INTEGER REFERENCES memes ON DELETE CASCADE, image_tag_id INTEGER REFERENCES image_tags ON DELETE CASCADE, created_at TIMESTAMP DEFAULT now());"),
    step("CREATE UNIQUE INDEX i_memes_image_tags_id ON memes_image_tags USING btree (meme_id, image_tag_id);")
]
