from __future__ import print_function

import os,re,sys
# tests/runner.py
import unittest

# FIXME: request new qtree and move splunk_sdk to /org/seg/tools/eda/splunk
SPLUNK_SDK_PATH = '/org/seg/tools/eda/apple/splunk/splunk_sdk_python/latest'
splunk_sdk_path = os.getenv('SPLUNK_SDK_PATH', SPLUNK_SDK_PATH)

if not os.path.isdir(splunk_sdk_path):
  raise Exception('Unable to find Splunk SDK Python library: ' + splunk_sdk_path)

sys.path.append(splunk_sdk_path)
sys.path.append(os.path.join(os.path.dirname(__file__)))

# Add the splunk_capture.py folder path to the sys.path list
sys.path.append('../..')

# import test modules
import test_filereader
import test_utils
import test_action
import test_main

# initialize the test suite
loader = unittest.TestLoader()
suite  = unittest.TestSuite()

# add tests to the test suite
suite.addTests(loader.loadTestsFromModule(test_main))
suite.addTests(loader.loadTestsFromModule(test_filereader))
suite.addTests(loader.loadTestsFromModule(test_utils))
suite.addTests(loader.loadTestsFromModule(test_action))

# initialize runner, pass it suite and run it
runner = unittest.TextTestRunner(verbosity=3)
result = runner.run(suite)
