"""
 Drop seperate token columns in users
"""

from yoyo import step

__depends__ = {'20160415_02_eqYBZ-alter-notifications-table'}

steps = [
    step("ALTER TABLE users DROP COLUMN apple_token"),
    step("ALTER TABLE users DROP COLUMN android_token")
]
