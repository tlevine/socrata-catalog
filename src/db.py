#!/usr/bin/env python
import os, json
from dumptruck import DumpTruck

dt = DumpTruck(dbname = '/tmp/catalog.db')

# Create a unique index on `identifier`.
dt.execute('''
CREATE TABLE IF NOT EXISTS "catalog" (
  "portal" TEXT NOT NULL,
  "identifier" TEXT NOT NULL,
  PRIMARY KEY ("portal", "identifier")
);''')

for data_json in os.listdir('catalogs'):
    # Load into memory.
    data = json.load(open(os.path.join('catalogs', data_json)))[1:]

    # Add the portal.
    portal = data_json.replace('.json', '')
    for row in data:
        row['portal'] = portal

    # Put in the database.
    dt.insert(data, 'catalog')
