.PHONY: download
download:
	src/download.sh

db: download
	src/db.py
