import unittest

import sys
import os
import filecmp
import argparse

from splunk_capture import *

# testing different argument combinations
# making sure required arguments are passed
# can add more tests once dryrun option is added
class ActionTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        pass
        #print('setupClass')


    @classmethod
    def tearDownClass(cls):
        pass
        #print('teardownClass')

    def setUp(self):
        global logger
        import logging.config
        logging.config.fileConfig(LOGGING_CONFIG_FILE)
        logger = logging.getLogger(__name__)

        parser = argparse.ArgumentParser()
        subparsers = parser.add_subparsers()

        self.event_parser = subparsers.add_parser('event')
        self.event_parser.add_argument("--index", help="splunk index")
        self.event_parser.add_argument("--project", help="project", default=os.getenv('PROJECT'))
        self.event_parser.add_argument("--flow", help="flow", default='main')
        self.event_parser.add_argument("--input", help="input file to read (supported formats: .csv, .xlsx, .mxd, .json)")
        self.event_parser.add_argument("--input_type", help="treat input as this type (supported formats: csv, xlsx, mxd, json)", default=None)
        self.event_parser.add_argument("--output_mode", help="result output format (supported formats: xml, json, json_cols, json_rows, csv)", default='json')
        self.event_parser.add_argument("--monitor", help="use splunk file monitoring mode", dest='monitor', action='store_true')
        self.event_parser.add_argument("--source", help="splunk source", default=os.path.basename(__file__))
        self.event_parser.add_argument("--sourcetype", help="splunk source type", default=SOURCETYPE)
        self.event_parser.add_argument("--host", help="splunk host", default=HOST)
        self.event_parser.add_argument("--hec_host", help="splunk hec host", default=HEC_HOST)
        self.event_parser.add_argument("--hec_token", help="splunk hec token", default=HEC_TOKEN)
        self.event_parser.add_argument("--extra", help="additional metadata to attach to events or search criteria. Format: JSON string")
        self.event_parser.add_argument("--limit", help='limit number of events extracted to this number. Default: 1000')
        self.event_parser.add_argument("--select", help="run query to display events in a single index. Outputs _raw and _time fields only. Use --search option for unrestricted search")
        self.event_parser.add_argument("--search", help="run any splunk search command and display the results")
        self.event_parser.add_argument("--list_indexes", help="list Splunk indexes you have access to", action='store_true')
        self.event_parser.add_argument("--app", help="splunk app namespace")
        self.event_parser.add_argument("--force", help=argparse.SUPPRESS, action='store_true')
        self.event_parser.add_argument("--verbose", help="show more details", action='store_true')
        self.event_parser.add_argument("--nometa", help="do not add metadata to event", action='store_true')
        self.event_parser.add_argument("--rc", help="use credentials from .rc file in home dir (.splunkrc)", action='store_true', default=False)

        self.kvstore_parser = subparsers.add_parser('kvstore')
        self.kvstore_parser.add_argument("--app", help="splunk kvstore app namespace", required=True)
        self.kvstore_parser.add_argument("--collection", help="splunk kvstore collection name", default='None')
        self.kvstore_parser.add_argument("--owner", help="splunk kvstore owner")
        self.kvstore_parser.add_argument("--input", help="input JSON file or JSON string")
        self.kvstore_parser.add_argument("--input_type", help="treat input as this type (supported formats: csv, xlsx, mxd, json)", default=None)
        self.kvstore_parser.add_argument("--list", help="list all collections in the app namespace", action='store_true')
        self.kvstore_parser.add_argument("--host", help="splunk host", default=HOST)
        self.kvstore_parser.add_argument("--select", help="read data from a collection")
        self.kvstore_parser.add_argument("--selectall", help="read entire data from a collection", action='store_true')
        self.kvstore_parser.add_argument("--update", help="update data in a collection", action='store_true')    
        self.kvstore_parser.add_argument("--delete", help="delete selected data in a collection")
        self.kvstore_parser.add_argument("--deleteall", help="CAUTION: deletes entire data in a collection", action='store_true')
        self.kvstore_parser.add_argument("--batch_delete", help="batch delete data via input file", action='store_true')
        self.kvstore_parser.add_argument("--extra", help="additional metadata to attach to documents. Format: json string")
        self.kvstore_parser.add_argument("--rc", help="use credentials from .rc file in home dir (.splunkrc)", action='store_true', default=False)
        self.kvstore_parser.set_defaults(func=process_kvstore)

        self.csv_parser = subparsers.add_parser('lookup')
        self.csv_parser.add_argument("--app", help="splunk CSV lookup app namespace", required=True)
        self.csv_parser.add_argument("--input", help="input CSV lookup file", required=True)
        self.csv_parser.add_argument("--schema", help="JSON schema to validate the CSV lookup file ", required=True)
        self.csv_parser.add_argument("--lint", help="validate the input CSV lookup file. JSON schema is required.", default=False, action='store_true')
        self.csv_parser.add_argument("--monitor", help="validate and monitor CSV file", default=True, action='store_true')
        self.csv_parser.add_argument("--force", help=argparse.SUPPRESS, action='store_true')
        self.csv_parser.add_argument("--verbose", help="show more details", action='store_true')
        self.csv_parser.add_argument("--host", help="splunk host", default=HOST)
        self.csv_parser.add_argument("--rc", help="use credentials from .rc file in home dir (.splunkrc)", action='store_true', default=False)

        #print('setUp')

    def tearDown(self):
        pass
        #print('tearDown')

    def test_event_args(self):
        # can repeat tests from other test modules if necessary
        event_args = self.event_parser.parse_args(['--list', '--rc'])
        process_event(event_args)

        # test everything valid
        event_args = self.event_parser.parse_args(['--index', 'playground1'])
        # test everything not valid

        event_args = self.event_parser.parse_args(['--input', 'test.json'])

    def test_kvstore_args(self):
        # can repeat tests from other test modules if necessary
        kvstore_args = self.kvstore_parser.parse_args(['--app', 'mxd3', '--collection', 'pd_test23', '--rc'])
        # test everything valid
        self.kvstore_parser.parse_args(['--app', 'mxd3', '--collection', 'pd_test23', '--list', '--rc'])
        # test everything not valid

    def test_lookup_args(self):
        # can repeat tests from other test modules if necessary
        lookup_args = self.csv_parser.parse_args(['--app', 'mxd3', '--input', '../input_data/targets.csv', '--schema', '../input_data/targets_schema.json'])
        # test everything valid
        # should fail because of missing schema

        with self.assertRaises(Exception) as raises_e:
            print(str(raises_e))
            self.csv_parser.parse_args(['--app', 'mxd3', '--input', '../input_data/targets.csv', '--rc'])
        # test everything not valid

if __name__ == '__main__':
    unittest.main()
