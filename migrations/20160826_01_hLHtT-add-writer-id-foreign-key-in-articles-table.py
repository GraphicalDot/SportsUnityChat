"""
Add writer_id foreign key in articles table
"""

from yoyo import step

__depends__ = {}

steps = [
    step("ALTER TABLE articles ADD COLUMN article_writer text REFERENCES content_writers (username) ON DELETE CASCADE;"),
    step("ALTER TABLE articles ADD CONSTRAINT check_article_state CHECK (article_state IN ('Draft', 'UnPublished', 'Published'))"),
    step("ALTER TABLE articles ADD COLUMN article_notification_content text"),
    step("ALTER TABLE articles ALTER COLUMN article_publish_date DROP NOT NULL"),
    step("ALTER TABLE articles ALTER COLUMN article_publish_date DROP DEFAULT")
]
