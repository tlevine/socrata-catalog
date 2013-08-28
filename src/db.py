#!/usr/bin/env python
import os, json
from dumptruck import DumpTruck

dt = DumpTruck(dbname = '/tmp/catalog.db')

for data_json in os.listdir('catalogs'):
    # Load into memory.
    data = json.load(open(os.path.join('catalogs', data_json)))[1:]

    # Add the portal.
    for row in data:
        data['portal'] = data_json.replace('.json', '')

    # Create a unique index on `identifier`.
    dt.create_table(data[0], 'catalog', if_not_exists = True)
    dt.create_index(['identifier'], 'catalog', unique = True, if_not_exists = True)

    # Put in the database.
    dt.upsert(data, 'catalog')
