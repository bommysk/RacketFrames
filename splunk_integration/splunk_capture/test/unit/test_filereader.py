
import unittest

# imports needed for tests
from sclib.filereader import FileReaderFactory, JSONFileReader, CSVFileReader, XLSFileReader, MXDFileReader

class FilereaderTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        pass
        #print('setupClass')

    @classmethod
    def tearDownClass(cls):
        pass
        #print('teardownClass')

    def setUp(self):
        self.small_input_json = '../input_data/small_input.json'
        self.small_input_csv = '../input_data/small_input.csv'
        self.small_input_xlsx = '../input_data/small_input.xlsx'
        self.small_input_mxd = '../input_data/small_input.mxd'
        #print('setUp')

    def tearDown(self):
        pass
        #print('tearDown')

    def test_json_reader(self):
        self.assertEqual(FileReaderFactory(self.small_input_json).parse_file(extra={}), 
        	[{'Founded': '1976', 'City': 'Cupertino', 'Name': 'Apple', '_key': 'apple_test_data',}])

    def test_csv_reader(self):
        self.assertEqual(FileReaderFactory(self.small_input_csv).parse_file(extra={}), 
        	[{u'Founded': 1976, u'City': u'Cupertino', u'Name': u'Apple'}])

    def test_xlsx_reader(self):
        self.assertEqual(FileReaderFactory(self.small_input_xlsx).parse_file(extra={}), 
         	[{u'founded': 1976, u'city': u'Cupertino', u'name': u'Apple'}])

    def test_mxd_reader(self):
        self.assertEqual(FileReaderFactory(self.small_input_mxd).parse_file(extra={}), 
        	[{u'Founded': u'1976', u'City': u'Cupertino', u'Name': u'Apple', u'SYS/MXD_FILE': u'../input_data/small_input.mxd'}])

    def test_json_reader_with_extra(self):
        self.assertEqual(FileReaderFactory(self.small_input_json).parse_file(extra={'a':1,'b':2}),
            [{'Founded': '1976', 'City': 'Cupertino', 'Name': 'Apple', 'a': 1, 'b': 2, '_key': 'apple_test_data',}])

    def test_csv_reader_with_extra(self):
        self.assertEqual(FileReaderFactory(self.small_input_csv).parse_file(extra={'a':1,'b':2}),
            [{u'Founded': 1976, u'City': u'Cupertino', u'Name': u'Apple', 'a': 1, 'b': 2}])

    def test_xlsx_reader_with_extra(self):
        self.assertEqual(FileReaderFactory(self.small_input_xlsx).parse_file(extra={'a':1,'b':2}),
            [{u'founded': 1976, u'city': u'Cupertino', u'name': u'Apple', 'a': 1, 'b': 2}])

    def test_mxd_reader_with_extra(self):
        self.assertEqual(FileReaderFactory(self.small_input_mxd).parse_file(extra={'a':1,'b':2}),
            [{u'Founded': u'1976', u'City': u'Cupertino', u'Name': u'Apple', u'SYS/MXD_FILE': u'../input_data/small_input.mxd', u'a': 1, u'b': 2}])

    def test_get_json_reader(self):
        self.assertEqual(FileReaderFactory(self.small_input_json).get_reader(), JSONFileReader)

    def test_get_csv_reader(self):
        self.assertEqual(FileReaderFactory(self.small_input_csv).get_reader(), CSVFileReader)

    def test_get_xlsx_reader(self):
        self.assertEqual(FileReaderFactory(self.small_input_xlsx).get_reader(), XLSFileReader)

    def test_get_mxd_reader(self):
        self.assertEqual(FileReaderFactory(self.small_input_mxd).get_reader(), MXDFileReader)

if __name__ == '__main__':
    unittest.main()
