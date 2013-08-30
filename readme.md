Socrata catalogs
=====
Learn things from the data catalog JSON file on Socrata portals.

This file is at [`/data.json`](https://data.oregon.gov/data.json),
and it has [this format](http://project-open-data.github.io/schema/).

## Database
Download the files, and put them in an SQLite3 database.

```sh
make db
```

The files will be in `./catalogs`, and the SQLite3 database will be in
`/tmp/catalogs.db`. Here are some fun queries.

```sql
SELECT format, count(*) FROM catalog GROUP BY format ORDER BY count(*);
SELECT portal, format, count(*) FROM catalog GROUP BY portal, format ORDER BY count(*);
SELECT portal, count(*) FROM catalog WHERE format = 'application/vnd.ms-excel' GROUP BY portal ORDER BY portal;
SELECT 'https://' || portal || '/-/-/' || identifier AS url, title from catalog where format LIKE '%excel%';
SELECT 'https://' || portal || '/-/-/' || identifier AS url, title FROM catalog WHERE portal = 'data.sfgov.org' AND format = 'application/octet-stream';
SELECT portal, format, count(*) FROM catalog GROUP BY portal, format ORDER BY portal, format;
```

## Analyses
Run any of the various analyses.

* `make formats`: What file formats do source data come from?
* `make external`: What external files are linked?
