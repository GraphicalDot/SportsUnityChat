"""
Add article_group_name column in articles table
"""

from yoyo import step

__depends__ = {"20160803_01_kErzw-add-curated-articles-tables"}

steps = [
    step("ALTER TABLE articles ADD COLUMN article_group_name text NOT NULL DEFAULT 'Article Group';")
]

