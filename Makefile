.PHONY: download
download:
	test -e catalogs || src/download.sh

db: download
	src/db.py

formats: db
	cd src && Rscript format-over-time.r
