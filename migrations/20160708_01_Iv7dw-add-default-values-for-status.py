"""

"""

from yoyo import step

__depends__ = {}

steps = [
    step("ALTER TABLE users ADD CONSTRAINT DEFAULT 'Available' FOR status")
]
