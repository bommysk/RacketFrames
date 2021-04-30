# splunk_capture

## Introduction
splunk_capture is a command line tool that can be used to run:

* (Create/Read) operations on Splunk index.
* (Create/Read/Update/Delete) operations on kvstore.
* (Create/Read/Update) operations csv lookup.

By default all data is injected into Splunk host: https://insights.csg.apple.com.

### Prerequisites

OD access to the default Splunk host.
read/write access to the indexes or kvstore collections.

### Installing

For the dev installation simply clone the repo in your CE sandbox or personal VM.

```
git clone gitlab@gitlab.csg.apple.com:splunk-cli/splunk_capture.git
```

Examples of all operations can be found here: [splunk_capture user documentation](https://seg-confluence.csg.apple.com/display/CAD/Insights+CLI%3A+splunk_capture).

## Running the tests

```
% cd test/functional

# run the tests
% ./test -load_list=test_list.txt

# generate golden results
./generate_golden_results -load_list=test_list.txt

# view test frame output without overwriting golden results
% ./generate_golden_results -load_list=test_list.txt -no_write
```

### Break down into end to end tests

These tests verify all major functionality of splunk_capture, the full test frame must pass before it can be released.

Example:
```
****************************************
testing golden_results/kvstore_batch_delete_test
****************************************
tests/kvstore_batch_delete_test_custom found: | grep -oP "('status': \d\d\d)"
./splunk_capture kvstore --app mxd3 --collection pd_test23 --delete --input test/input_data/small_input_delete.json | grep -oP "('status': \d\d\d)"

diff build/test/results/kvstore_batch_delete_test golden_results/kvstore_batch_delete_test
PASSED
----------------------------------------
```

## Deployment

Please reference the deployment script to release splunk_capture into CE.
```
/home/socpuppet/bin/splunk/deploy_splunk_capture.sh
```

## Built With

* [Python 2.7](https://docs.python.org/2) - The default version on CentOS 6 is 2.7.11 and on CentOS 7 it is 2.7.14. Python 2 is at EOL, in the future splunk_capture will be updated to Python 3.
* [Splunk Python SDK](https://dev.splunk.com/enterprise/docs/python/sdk-python) - Used for all operations besides event injection.
* [SplunkIt](https://scv-gitlab.csg.apple.com/splunk-cli/SplunkIt) - Used for event injection.

## Contributing

Please read [splunk_capture developer documentation](https://seg-confluence.csg.apple.com/display/CAD/Insights+CLI+Developer+Document%3A+splunk_capture?src=contextnavpagetreemode) for details on our code of conduct, and the process for submitting pull requests.

## Versioning

We simply increment the version on each new release. For example if the current version is v014, the next version will be v015. The version is configured through the deployment script.

## Authors

* **Shubham Kahal** - (skahal@apple.com)
* **Hemanth Singamsetty** - (hemanth@apple.com)
