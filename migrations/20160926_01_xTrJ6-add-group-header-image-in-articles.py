"""
Add group header image in articles
"""

from yoyo import step

__depends__ = {}

steps = [
    step("ALTER TABLE articles ADD COLUMN article_group_header_image TEXT;")
]
