import unittest

# imports needed for tests
from sclib.utils import *
from scsettings.common import *

import os

class UtilsTest(unittest.TestCase):
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

        #print('setUp')

    def tearDown(self):
        pass
        #print('tearDown')

    def test_filter_dict_by_keys(self):
        test_dict = {'a': 1, 'b': 2, 'c': 3}

        self.assertEqual(filter_dict_by_keys(test_dict, []), {})
        self.assertEqual(filter_dict_by_keys(test_dict, ['a']), {'a': 1})
        self.assertEqual(filter_dict_by_keys(test_dict, ['b', 'c']), {'b': 2, 'c': 3})
        self.assertEqual(filter_dict_by_keys(test_dict, ['a', 'c']), {'a': 1, 'c': 3})
        self.assertEqual(filter_dict_by_keys(test_dict, ['a', 'b', 'c']), test_dict)

    def test_is_list_of_strings(self):
        l = ['abc', 'def', 'ghi', 'jkl', 'mno']

        self.assertTrue(is_list_of_strings(l))

        l = ['abc', 10, 'ghi', 25.2, 'mno']        

        self.assertFalse(is_list_of_strings(l))

    def test_is_list_of_dicts(self):
        d = [{'a': 1, 'b': 2, 'c': 3}, {'a': 1, 'b': 2, 'c': 3}, {'a': 1, 'b': 2, 'c': 3}]

        self.assertTrue(is_list_of_dicts(d))

        d = [{'a': 1, 'b': 2, 'c': 3}, {'a': 1, 'b': 2, 'c': 3}, 'ghi', 25.2, {'a': 1, 'b': 2, 'c': 3}]        

        self.assertFalse(is_list_of_dicts(d))

    def test_json2dict(self):
        self.assertEqual(json2dict('{"a": 1, "b": 2, "c": 3}'), [{'a': 1, 'b': 2, 'c': 3}])
        self.assertEqual(json2dict('[{"a": 1, "b": 2, "c": 3}, {"e": 4, "f": 5, "g": 6}]'), [{'a': 1, 'b': 2, 'c': 3}, {"e": 4, "f": 5, "g": 6}])
        self.assertEqual(json2dict('{"a": 1, "b": 2, "c": 3}', {"e": 4, "f": 5, "g": 6}), [{'a': 1, 'b': 2, 'c': 3, "e": 4, "f": 5, "g": 6}])


    def test_get_curtime(self):
        try:
            get_curtime()
        except Exception as inst:
            print type(inst)     # the exception instance
            print inst.args      # arguments stored in .args
            print inst           # __str__ allows args to be printed directly

    def test_gen_uuid(self):
        try:
            gen_uuid()
        except Exception as inst:
            print type(inst)     # the exception instance
            print inst.args      # arguments stored in .args
            print inst           # __str__ allows args to be printed directly

    def test_get_od_user(self):
        try:
            get_od_user()
        except Exception as inst:
            print type(inst)     # the exception instance
            print inst.args      # arguments stored in .args
            print inst           # __str__ allows args to be printed directly

    def get_site(self):
        try:
            get_site()
        except Exception as inst:
            print type(inst)     # the exception instance
            print inst.args      # arguments stored in .args
            print inst           # __str__ allows args to be printed directly

    def test_syscmd(self):
        try:
            syscmd('echo syscmd echo test')
        except Exception as inst:
            print type(inst)     # the exception instance
            print inst.args      # arguments stored in .args
            print inst           # __str__ allows args to be printed directly

if __name__ == '__main__':
    unittest.main()
