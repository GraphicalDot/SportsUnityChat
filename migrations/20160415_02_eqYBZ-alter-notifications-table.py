"""
Alter notifications table
"""

from yoyo import step


steps = [
	step(" ALTER TABLE notifications ADD COLUMN match_id text "),
	step(" UPDATE notifications SET gcm_response = '{}' WHERE gcm_response = 'None'"),
	step(" ALTER TABLE notifications ALTER COLUMN notification TYPE JSON USING notification::json"),
	step(" ALTER TABLE notifications ALTER COLUMN gcm_response TYPE JSON USING gcm_response::json"),
	step(" ALTER TABLE notifications ADD COLUMN error TEXT"),
]


