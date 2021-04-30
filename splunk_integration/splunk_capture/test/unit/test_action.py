import unittest

import sys
import os
import filecmp
import argparse
import time

# imports needed for tests
from sclib.action import *
from sclib.filereader import FileReaderFactory

def read_input(args, meta=None):
    params = {}
    if meta is not None:
        params.update(meta)
    if args["extra"]:
        params.update(json.loads(args["extra"]))
    return FileReaderFactory(args["input"], args["input_type"]).parse_file(extra=params)

TEST_MONITOR_INDEX = "prj_skua_main"
TEST_INDEX = "playground1"

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
        #print('setUp')

        global logger
        import logging.config
        logging.config.fileConfig(LOGGING_CONFIG_FILE)
        logger = logging.getLogger(__name__)

        # Wait for 5 seconds
        #time.sleep(5)

        test_kwargs = {}
        test_kwargs["host"] = HOST

        select_kwargs = {}
        select_kwargs["host"] = HOST
        select_kwargs["app"] = DEFAULT_APP
        # disable preview which results in duplicate results
        select_kwargs["preview"] = False
        select_kwargs["output_mode"] = 'json'

        self.splunk_service = SplunkService(True, **test_kwargs)
        self.splunk_search = SplunkSearch(True, **select_kwargs)        
        self.splunk_index = SplunkIndex(TEST_INDEX, True, **test_kwargs)
        self.splunk_index_arbitrary = SplunkIndex(None, True, **test_kwargs)

        self.splunk_event_args = {'index': TEST_INDEX, 'source': 'splunk_capture_test', 'sourcetype': '_json', 'keep_deleted': False, 'hec_host': 'https://scv-splunk-segue-fwd01.csg.apple.com:8088', 'project': None, 'flow': None, 'rc': True}
        
        self.splunkit_index = SplunkItIndex(self.splunk_event_args)
        self.splunk_event = SplunkEvent(self.splunk_event_args, **test_kwargs)

        kvstore_parser = argparse.ArgumentParser()
        kvstore_parser.add_argument("--app", help="splunk kvstore app namespace", required=True)
        kvstore_parser.add_argument("--collection", help="splunk kvstore collection name", default='None')
        kvstore_parser.add_argument("--owner", help="splunk kvstore owner")
        kvstore_parser.add_argument("--input", help="input JSON file or JSON string")
        kvstore_parser.add_argument("--input_type", help="treat input as this type (supported formats: csv, xlsx, mxd, json)", default=None)
        kvstore_parser.add_argument("--list", help="list all collections in the app namespace", action='store_true')
        kvstore_parser.add_argument("--host", help="splunk host", default=HOST)
        kvstore_parser.add_argument("--select", help="read data from a collection")
        kvstore_parser.add_argument("--selectall", help="read entire data from a collection", action='store_true')
        kvstore_parser.add_argument("--update", help="update data in a collection", action='store_true')    
        kvstore_parser.add_argument("--delete", help="delete selected data in a collection")
        kvstore_parser.add_argument("--deleteall", help="CAUTION: deletes entire data in a collection", action='store_true')
        kvstore_parser.add_argument("--batch_delete", help="batch delete data via input file", action='store_true')
        kvstore_parser.add_argument("--extra", help="additional metadata to attach to documents. Format: json string")
        kvstore_parser.add_argument("--rc", help="use credentials from .rc file in home dir (.splunkrc)", action='store_true', default=False)

        # supported fields: _key, CustID, CustName, CustStreet, CustState, CustCity, CustZip
        self.lookup = 'pd_test23'
        
        kvstore_args = kvstore_parser.parse_args(['--app', 'mxd3', '--collection', self.lookup, '--rc'])

        #print(kvstore_args)

        self.splunk_kvstore = SplunkKVStore(kvstore_args)   

        app = 'megacell'
        csv_file_name = '../input_data/targets.csv'
        csv_file_schema = '../input_data/targets_schema.json'
        bad_csv_file_schema = '../input_data/targets_schema.json'
        csv_content = FileReaderFactory(csv_file_name).parse_file()
        csv_schema = FileReaderFactory(csv_file_schema).parse_file()
        rc = True
        
        self.csv_obj = SplunkCSV(app, csv_file_name, csv_content, csv_schema, rc, **test_kwargs)

    def tearDown(self):
        #print('tearDown')
        #print('ending Splunk sessions')
        self.splunk_service.service.logout()
        self.splunk_search.service.logout()
        self.splunk_index.service.logout()
        self.splunk_index_arbitrary.service.logout()        
        self.splunk_event.service.logout()

    ### SplunkSearch ###
    def test_list_all_indexes(self):
        self.assertTrue(self.splunkit_index.list_indexes())

    def test_run_search(self):
        select_kwargs = {}
        select_kwargs["host"] = HOST
        select_kwargs["app"] = DEFAULT_APP
        # disable preview which results in duplicate results
        select_kwargs["preview"] = False
        select_kwargs["output_mode"] = 'json'      
        self.assertTrue(len(self.splunk_search.run_search('search index=' + TEST_INDEX + ' earliest=1 | head 10 | table _raw, _time', **select_kwargs).split("\n")) == 10)

    ### SplunkIndex ###
    def test_is_valid_index(self):
        self.assertTrue(self.splunkit_index.is_valid_index())
        prev_index_name = self.splunkit_index.index
        self.splunkit_index.index = 'foo'
        self.assertFalse(self.splunkit_index.splunkit_index())
        self.splunkit_index.index = prev_index_name

    def test_search(self):
        select_kwargs = {}
        select_kwargs["host"] = HOST
        select_kwargs["app"] = DEFAULT_APP
        # disable preview which results in duplicate results
        select_kwargs["preview"] = False
        select_kwargs["output_mode"] = 'json'
        self.assertTrue(len(self.splunk_index.search('index=' + TEST_INDEX + ' earliest=1', 10, **select_kwargs).split("\n")) == 10)

    def test_monitor_index(self):
        path = '../input_data/events.json'
        args = {'input': path, 'index' : TEST_MONITOR_INDEX, 'montor' : True, 'extra' : None}        
        result = SplunkEvent.insert(read_input(args), args)
        self.assertTrue(result == True)

    def test_monitor_project(self):
        path = '../input_data/events.json'
        args = {'input': path, 'project' : 'skua', 'montor' : True, 'extra' : None}
        result = SplunkEvent.insert(read_input(args), args)
        self.assertTrue(result == True)

    def test_prepare_query(self):
        expected = 'search index=' + TEST_INDEX + ' field1=foo field2=bar | head 100'
        expected_arbitrary = 'search field1=foo field2=bar | head 100'

        self.assertEqual(self.splunk_index.prepare_query('field1=foo field2=bar', 100), expected)
        self.assertEqual(self.splunk_index_arbitrary.prepare_query('field1=foo field2=bar', 100), expected_arbitrary)

        expected = 'search index=' + TEST_INDEX + ' field1=foo field2=bar | head 1000'
        expected_arbitrary = 'search field1=foo field2=bar | head 1000'

        self.assertEqual(self.splunk_index.prepare_query('field1=foo field2=bar', -10), expected)
        self.assertEqual(self.splunk_index_arbitrary.prepare_query('field1=foo field2=bar', -10), expected_arbitrary)

        expected = 'search index='+ TEST_INDEX + ' field1=foo field2=bar | head 1000'
        expected_arbitrary = 'search field1=foo field2=bar | head 1000'

        self.assertEqual(self.splunk_index.prepare_query('field1=foo field2=bar'), expected)
        self.assertEqual(self.splunk_index_arbitrary.prepare_query('field1=foo field2=bar'), expected_arbitrary)

        expected = 'search index='+ TEST_INDEX + ' field1=foo field2=bar | head 1000'
        expected_arbitrary = 'search field1=foo field2=bar | head 1000'

        self.assertEqual(self.splunk_index.prepare_query('search field1=foo field2=bar'), expected)
        self.assertEqual(self.splunk_index_arbitrary.prepare_query('search field1=foo field2=bar'), expected_arbitrary)

        expected = 'search index='+ TEST_INDEX + ' | stats count as successes by successful_transactions | head 1000'
        expected_arbitrary = '| stats count as successes by successful_transactions | head 1000'

        self.assertEqual(self.splunk_index.prepare_query('| stats count as successes by successful_transactions'), expected)
        self.assertEqual(self.splunk_index_arbitrary.prepare_query('| stats count as successes by successful_transactions'), expected_arbitrary)

    def test_get_search_query(self):
        self.assertEqual(self.splunk_event.get_search_query({'field1':'foo', 'field2':'bar'}), '"field1"="foo" "field2"="bar" "metadata__.oduser"="skahal" "metadata__.project"="common" "source"="splunk_capture_test" "sourcetype"="_json"')

    def test_select(self):
        select_kwargs = {}
        select_kwargs["host"] = HOST
        select_kwargs["app"] = DEFAULT_APP
        # disable preview which results in duplicate results
        select_kwargs["preview"] = False
        select_kwargs["output_mode"] = 'json'
        self.assertEqual(self.splunk_event.select('"field1"="foo" "field2"="bar"', **select_kwargs), None)

    def test_is_locked(self):
        self.assertFalse(SplunkIndex.is_locked(TEST_INDEX))
        self.assertTrue(SplunkIndex.is_locked('mxd_sicily_small'))

    ### SplunkEvent ###
    def test_insert(self):
        json_data = [{'test_event': 1}, {'test_event': 2}, {'test_event': 3}]        
        splunk_event_args = {'index': TEST_INDEX, 'source': 'splunk_capture_test', 'sourcetype': '_json', 'keep_deleted': False, 'hec_host': 'https://scv-splunk-segue-fwd01.csg.apple.com:8088', 'project': None, 'flow': None, 'rc': True}        
        SplunkEvent.insert(json_data, splunk_event_args)

        try:
            self.assertTrue(self.splunk_index.search('source="splunk_capture_test" earliest=now-10s', 10))
        except Exception as inst:
            print type(inst)     # the exception instance
            print inst.args      # arguments stored in .args
            print inst           # __str__ allows args to be printed directly

    ### Splunk Kvstore ###
    def test_kvstore_collection_list(self):
        self.assertTrue(self.lookup in self.splunk_kvstore.list_collections())

    def test_kvstore_exists(self):
        self.assertTrue(self.splunk_kvstore.exists())

    def test_kvstore_batch_insert(self):
        documents = [{'_key': '544948df3ec32d7a4c1d9755', 'CustID': 1, 'CustName': 'Joe Smith', 'CustStreet': '1st Street', 'CustState': 'CA', 'CustCity': 'Fremont', 'CustZip': 91234}]
        self.assertTrue(len(self.splunk_kvstore.batch_insert(documents)) == len(documents))

    def test_kvstore_query(self):
        documents = [{'_key': '544948df3ec32d7a4c1d9755', 'CustID': 1, 'CustName': 'Joe Smith', 'CustStreet': '1st Street', 'CustState': 'CA', 'CustCity': 'Fremont', 'CustZip': 91234}]
        self.assertTrue(len(self.splunk_kvstore.batch_insert(documents)) == len(documents))
        self.assertTrue(self.splunk_kvstore.query())
        self.assertTrue(len(json.loads(self.splunk_kvstore.query('{"CustID": 1, "CustName": "Joe Smith"}'))) > 0)
        self.assertTrue(len(json.loads(self.splunk_kvstore.query('{"CustID": "foo", "CustName": "bar"}'))) == 0)

    def test_kvstore_batch_update(self):
        documents = [{'_key': '544948df3ec32d7a4c1d9755', 'CustID': 1, 'CustName': 'Joe Smith', 'CustStreet': '1st Street', 'CustState': 'CA', 'CustCity': 'Fremont', 'CustZip': 91234}]
        self.assertTrue(len(self.splunk_kvstore.batch_insert(documents)) == len(documents))
        documents = [{'_key': '544948df3ec32d7a4c1d9755', 'CustID': 1, 'CustName': 'Joe Smith Update', 'CustStreet': '1st Street Update', 'CustState': 'CA Update', 'CustCity': 'Fremont Update', 'CustZip': 91234}]
        # testing if all documents updated
        self.assertTrue(len(self.splunk_kvstore.batch_update(documents)) == len(documents))

    # intermittently failing
    def test_kvstore_query_by_id(self):
        documents = [{'_key': '544948df3ec32d7a4c1d9755', 'CustID': 1, 'CustName': 'Joe Smith', 'CustStreet': '1st Street', 'CustState': 'CA', 'CustCity': 'Fremont', 'CustZip': 91234}]
        self.assertTrue(len(self.splunk_kvstore.batch_insert(documents)) == len(documents))
        print(self.splunk_kvstore.query_by_id('544948df3ec32d7a4c1d9755'))
        self.assertTrue(len(json.loads(self.splunk_kvstore.query_by_id('544948df3ec32d7a4c1d9755'))) > 0)
        #self.assertTrue(len(json.loads(self.splunk_kvstore.query_by_id('544948df3ec32d7a4c1d9756'))) == 0)

    def test_kvstore_batch_delete(self):
        documents = [{'_key': '544948df3ec32d7a4c1d9755', 'CustID': 1, 'CustName': 'Joe Smith', 'CustStreet': '1st Street', 'CustState': 'CA', 'CustCity': 'Fremont', 'CustZip': 91234}]
        self.assertTrue(len(self.splunk_kvstore.batch_insert(documents)) == len(documents))
        documents = [{'_key': '544948df3ec32d7a4c1d9755', 'CustID': 1, 'CustName': 'Joe Smith', 'CustStreet': '1st Street', 'CustState': 'CA', 'CustCity': 'Fremont', 'CustZip': 91234}]
        # testing if all documents deleted
        self.assertTrue(len(self.splunk_kvstore.batch_delete(documents)) == len(documents))

    def test_kvstore_delete_by_query(self):
        self.assertTrue(self.splunk_kvstore.delete_by_query('{"CustID": 1, "CustName": "Joe Smith Update"}')['status'] == 200)

    def test_kvstore_is_valid_data(self):
        self.assertTrue(self.splunk_kvstore.is_valid_data([{'_key': '544948df3ec32d7a4c1d9755', 'CustID': 1, 'CustName': 'Joe Smith', 'CustStreet': '1st Street', 'CustState': 'CA', 'CustCity': 'Fremont', 'CustZip': 91234}]))
        # missing '_key'
        self.assertFalse(self.splunk_kvstore.is_valid_data([{'CustID': 1, 'CustName': 'Joe Smith', 'CustStreet': '1st Street', 'CustState': 'CA', 'CustCity': 'Fremont', 'CustZip': 91234}]))

    def test_csv_lint(self):
        self.assertTrue(self.csv_obj.lint())

        # bad schema test
        test_kwargs = {}
        test_kwargs["host"] = HOST
        app = 'megacell'
        csv_file_name = '../input_data/targets.csv'
        csv_file_schema = '../input_data/targets_schema.json'
        bad_csv_file_schema = '../input_data/targets_bad_schema.json'
        csv_content = FileReaderFactory(csv_file_name).parse_file()
        csv_schema = FileReaderFactory(csv_file_schema).parse_file()
        rc = True

        with self.assertRaises(ValueError) as raises_ve:
            SplunkCSV(app, csv_file_name, csv_content, bad_csv_file_schema, rc, **test_kwargs)

    def test_csv_monitor(self):
        self.csv_obj.monitor()

if __name__ == '__main__':
    unittest.main()
