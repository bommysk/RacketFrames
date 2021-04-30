import unittest
# Now you can import your module
#from splunk_capture import main

from splunk_capture import *

def raises_error(*args, **kwds):
    raise ValueError('Invalid value: %s%s' % (args, kwds))

class ExceptionTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        print('setupClass')

    @classmethod
    def tearDownClass(cls):
        print('teardownClass')

    def setUp(self):
        print('setUp')
        global logger
        import logging.config
        logging.config.fileConfig(LOGGING_CONFIG_FILE)
        logger = logging.getLogger(__name__)

    def tearDown(self):
        print('tearDown\n')

    def test_assert_raises(self):
        self.assertRaises(ValueError, process_event, ['splunk_capture', 'event'])

if __name__ == '__main__':
    unittest.main()
