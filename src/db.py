#!/usr/bin/env python
import os, json
from dumptruck import DumpTruck

dt = DumpTruck(dbname = '/tmp/catalog.db')

for data_json in os.listdir('catalogs'):
    data = json.load(open(os.path.join('catalogs', data_json)))[1:]
    dt.create_table(data[0], 'catalog', if_not_exists = True)
    dt.create_index(['identifier'], unique = True, if_not_exists = True)
    dt.upsert(data, 'catalog')
