"""

"""

from yoyo import step

__depends__ = {'20160802_01_d4s6I-add-articles-table'}

steps = [
    step("CREATE TABLE users_poll_responses (username TEXT REFERENCES users ON DELETE CASCADE, article_id INTEGER REFERENCES articles ON DELETE CASCADE, poll_answer CHAR NOT NULL, created_at TIMESTAMP DEFAULT now());"),
    step("CREATE UNIQUE INDEX i_username_article_id ON users_poll_responses USING btree (username, article_id);")

]
