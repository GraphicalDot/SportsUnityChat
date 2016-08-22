"""
Add default to articles state column
"""

from yoyo import step

__depends__ = {"20160803_01_kErzw-add-curated-articles-tables"}

steps = [
    step(" ALTER TABLE articles ALTER COLUMN article_state SET DEFAULT 'UnPublished';")
]
