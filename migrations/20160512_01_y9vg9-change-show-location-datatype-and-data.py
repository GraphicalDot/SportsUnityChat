"""
change show location datatype and data
"""

from yoyo import step


steps = [
    step("ALTER TABLE users ALTER COLUMN show_location TYPE char USING CASE WHEN show_location = True THEN 'a' ELSE 'n' END")
]
