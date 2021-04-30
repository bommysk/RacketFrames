#!/usr/bin/env python
from __future__ import print_function

import argparse
import json
import os
import re
import sys

if not os.getenv("SPLUNKIT_DIR"):
    os.environ["SPLUNKIT_DIR"] = "SplunkIt/installs"
if not os.getenv("SPLUNKIT_VERSION"):
    os.environ["SPLUNKIT_VERSION"] = "latest"

from sclib.action import SplunkItIndex, SplunkEvent, SplunkKVStore, SplunkCSV, SplunkSearch
from sclib.auth import set_user, set_passwd
from sclib.filereader import FileReaderFactory
from sclib.utils import *
import sclib.multiprocessing_logging
from scsettings.common import *

__author__ = 'Shubham Kahal'
__version__ = '1.0'

def read_input(args, meta=None):
    params = {}
    if meta is not None:
        params.update(meta)
    if args.extra:
        params.update(json.loads(args.extra))
    return FileReaderFactory(args.input, args.input_type).parse_file(extra=params)

def process_login(args):
    if args.password:
        set_passwd()
    if args.username:
        set_user(args.username)
        set_passwd()

def process_event(args):    
    try:
        kwargs = {}
        if args.force:
            kwargs["force"] = True
        if args.verbose:
            kwargs["verbose"] = True

        if args.host:
            kwargs['host'] = args.host

        if args.rc:
            kwargs['rc'] = args.rc
  
        if os.getenv('SPLUNK_CAPTURE_MONITOR') is not None:
            logger.info("Running splunk_capture in monitor mode") 
            args.monitor = True

        input_data = None        

        if args.list_indexes:
            print(SplunkItIndex(args.__dict__).list_indexes())
            return

        if args.input:
            if os.path.isfile(args.input):
                input_data = read_input(args) if args.nometa else read_input(args, get_metadata())
            else:
                raise IOError

        if args.select or args.search or args.search_input:
            event_obj = SplunkEvent(args.__dict__, **kwargs)
            
            select_args = {}
            if args.extra:
                select_args = json.loads(args.extra)

            select_args["app"] = DEFAULT_APP if args.app is None else args.app

            # disable preview which results in duplicate results
            select_args["preview"] = False
            select_args["output_mode"] = args.output_mode  

            limit = args.limit if args.limit else SEARCH_LIMIT

            if args.search:
                # set raw to True
                event_obj.select(args.search, True, limit, **select_args)
            elif args.select:
                event_obj.select(args.select, False, limit, **select_args)
            elif args.search_input:
                search_input = FileReaderFactory(args.search_input, 'json').parse_file()
                for search in search_input:
                    token_values = search["tokens"]
                    spl_template = read_template(search["spl"])
                    spl = spl_template.substitute(token_values)
                    event_obj.select(spl, True, limit, **select_args)
        elif input_data is not None:
            SplunkEvent.insert(input_data, args.__dict__)
    except IOError as ioe:
        abort(logger, 'Invaild input file - {}'.format(args.input))
    except ValueError as e:
        abort(logger, 'Invalid input value - {}'.format(str(e)))
    except TypeError as e:
        abort(logger, 'Invalid input type - {}'.format(str(e)))
    except Exception as e:
        abort(logger, str(e))

def process_kvstore(args):
    try:
        kwargs = {}

        if args.force:
            kwargs["force"] = True

        if args.host:
            kwargs['host'] = args.host

        if args.rc:
            kwargs['rc'] = args.rc

        if not (args.select or args.selectall or args.deleteall or args.list) and not args.input:
            logger.error('argument --input is required')
            raise ValueError

        if args.input:
            if os.path.isfile(args.input):
                data = read_input(args)
            else:
                data = json2dict(args.input, args.extra)
        
        kvstore_obj = SplunkKVStore(args, **kwargs)
        
        if args.list:
            print(kvstore_obj.list_collections())
        elif args.update:
            # update only the specified keys from CLI query
            print(kvstore_obj.update(data))
        elif args.update_append:
            # update only the specified keys from CLI query and preserve previous entry fields
            print(kvstore_obj.update_append(data))
        elif args.delete:
            # delete only the specified keys from CLI query
            print(kvstore_obj.delete(data))
        elif args.deleteall:
            # delete entire collection data            
            do_delete_all = "n"
            if args.force:
            	do_delete_all = "y"
            else:
                do_delete_all = raw_input("Are you sure you want to delete the entire collection: {}/{}? This process cannot be reverted. (y/n): ".format(kvstore_obj.app, args.collection))
            if (do_delete_all.lower() == "y" or do_delete_all.lower() == "yes"):
                try:
                    selectall_result = kvstore_obj.query()
                    selectall_file = open("selectall.bkp.json", "w")
                    n = selectall_file.write(selectall_result)
                    selectall_file.close()
                    logger.info("Saving backup in case restore is needed: {}".format("selectall.bkp.json"))
                    print(kvstore_obj.delete())
                except IOError as ioe:
                    abort(logger, 'Invaild file - {}'.format("selectall.bkp.json"))                
            else:
                logger.info("Not running deleteall.")
        elif args.select:
            print(kvstore_obj.query(args.select))
        elif args.selectall:
            print(kvstore_obj.query())
        else:
            print(kvstore_obj.batch_insert(data))
    except Exception as e:
        abort(logger, str(e))

def process_csv(args):
    try:
        if not os.path.isfile(args.input):
            logger.error('argument --input not valid file')
            raise ValueError

        if not os.path.isfile(args.schema):
            logger.error('argument --schema not valid file')
            raise ValueError

        kwargs = {}
        if args.force:
            kwargs["force"] = True
        if args.verbose:
            kwargs["verbose"] = True

        if args.host:
            kwargs['host'] = args.host

        if args.rc:
            kwargs['rc'] = args.rc

        csv_file_name = args.input

        csv_content = FileReaderFactory(args.input).parse_file()

        csv_schema = FileReaderFactory(args.schema).parse_file()        

        csv_obj = SplunkCSV(args.app, csv_file_name, csv_content, csv_schema, **kwargs)
        
        csv_obj.monitor()        

    except Exception as e:
        abort(logger, str(e))

def main(argv=None):
    global logger
    import logging.config
    # Attempt default configuration for logging, fail silently if there is an issue
    try:
        logging.config.fileConfig(LOGGING_CONFIG_FILE)
        sclib.multiprocessing_logging.install_mp_handler()
    except IOError:
        #disable logging for all levels (CRITICAL is highest defined in logging module)
        logging.disable(logging.CRITICAL)

    logger = logging.getLogger(__name__)


    # print env
    if os.getenv('SPLUNK_CLI_DEV') is not None:
        logger.debug("ENV:" + 'SPLUNK_CLI_DEV')
    if os.getenv('SPLUNK_CAPTURE_VERSION') is not None:
        logger.info("Running splunk_capture version: "+ os.getenv('SPLUNK_CAPTURE_VERSION'))

    if argv is None:
        argv = sys.argv
    args = read_args(argv[1:])
    logger.debug(os.getenv('USER')+' - '+' '.join(sys.argv))
    args.func(args)

def read_args(argv):
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers()

    login_parser = subparsers.add_parser('login')
    login_parser.add_argument("--username", help="update splunk username in local cache only")
    login_parser.add_argument("--password", help="update splunk password in local cache only", action='store_true')
    login_parser.set_defaults(func=process_login)

    event_parser = subparsers.add_parser('event')
    event_parser.add_argument("--index", help="splunk index")    
    event_parser.add_argument("--flow", help="flow", default='main')
    event_parser.add_argument("--input", help="input file to read (supported formats: .csv, .xlsx, .mxd, .json)")
    event_parser.add_argument("--input_type", help="treat input as this type (supported formats: csv, xlsx, mxd, json, monitor)", default=None)
    event_parser.add_argument("--output_mode", help="result output format (supported formats: xml, json, json_cols, json_rows, csv, monitor)", default='monitor')
    event_parser.add_argument("--monitor", help="use splunk file monitoring mode", dest='monitor', action='store_true')
    event_parser.add_argument("--source", help="splunk source", default=os.path.basename(__file__))
    event_parser.add_argument("--sourcetype", help="splunk source type", default=SOURCETYPE)
    event_parser.add_argument("--host", help="splunk host", default=HOST)
    event_parser.add_argument("--hec_host", help="splunk hec host", default=HEC_HOST)
    event_parser.add_argument("--hec_token", help="splunk hec token", default=HEC_TOKEN)
    event_parser.add_argument("--extra", help="additional metadata to attach to events or search criteria. Format: JSON string")
    event_parser.add_argument("--limit", help='limit number of events extracted to this number. Default: 1000')
    event_parser.add_argument("--select", help="run query to display events in a single index. Outputs _raw and _time fields only. Use --search option for unrestricted search")
    event_parser.add_argument("--search", help="run any splunk search command and display the results")
    event_parser.add_argument("--search_input", help="run any splunk search command from a file, good for long commands")
    event_parser.add_argument("--list_indexes", help="list Splunk indexes you have access to", action='store_true')
    event_parser.add_argument("--app", help="splunk app namespace")
    event_parser.add_argument("--force", help=argparse.SUPPRESS, action='store_true')
    event_parser.add_argument("--verbose", help="show more details", action='store_true')
    event_parser.add_argument("--nometa", help="do not add metadata to event", action='store_true')
    event_parser.add_argument("--rc", help="use credentials from .rc file in home dir (i.e. .splunkrc)")
    event_parser.set_defaults(func=process_event)

    kvstore_parser = subparsers.add_parser('kvstore')
    kvstore_parser.add_argument("--app", help="splunk kvstore app namespace", required=True)
    kvstore_parser.add_argument("--collection", help="splunk kvstore collection name", default='None')
    kvstore_parser.add_argument("--owner", help="splunk kvstore owner")
    kvstore_parser.add_argument("--input", help="input JSON file or JSON string")
    kvstore_parser.add_argument("--input_type", help="treat input as this type (supported formats: csv, xlsx, mxd, json)", default=None)
    kvstore_parser.add_argument("--list", help="list all collections in the app namespace", action='store_true')
    kvstore_parser.add_argument("--host", help="splunk host", default=HOST)
    kvstore_parser.add_argument("--select", help="read data from a collection")
    kvstore_parser.add_argument("--selectall", help="read entire data from a collection", action='store_true')    
    kvstore_parser.add_argument("--update", help="update data in a collection, will replace the entry mapping to _key with the one provided", action='store_true')
    kvstore_parser.add_argument("--update_append", help="update data in a collection but preserve all fields that are not being updated of the entry mapping to _key", action='store_true')
    kvstore_parser.add_argument("--delete", help="delete selected data in a collection", action='store_true')
    kvstore_parser.add_argument("--deleteall", help="CAUTION: deletes entire data in a collection", action='store_true')
    kvstore_parser.add_argument("--extra", help="additional metadata to attach to documents. Format: json string")
    kvstore_parser.add_argument("--force", help="force kvstore deleteall without prompt", action='store_true')
    kvstore_parser.add_argument("--rc", help="use credentials from .rc file in home dir (i.e. .splunkrc)")
    kvstore_parser.set_defaults(func=process_kvstore)

    csv_parser = subparsers.add_parser('lookup')
    csv_parser.add_argument("--app", help="splunk CSV lookup app namespace", required=True)
    csv_parser.add_argument("--input", help="input CSV lookup file", required=True)
    csv_parser.add_argument("--schema", help="JSON schema to validate the CSV lookup file ", required=True)
    csv_parser.add_argument("--monitor", help="validate and monitor CSV file", default=False, action='store_true')
    csv_parser.add_argument("--force", help=argparse.SUPPRESS, action='store_true')
    csv_parser.add_argument("--verbose", help="show more details", action='store_true')
    csv_parser.add_argument("--host", help="splunk host", default=HOST)
    csv_parser.add_argument("--rc", help="use credentials from .rc file in home dir (i.e. .splunkrc)")
    csv_parser.set_defaults(func=process_csv)

    return parser.parse_args(argv)

if __name__ == "__main__":
    main()
