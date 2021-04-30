from __future__ import print_function

from sclib.auth import get_splunk_cred_splunkrc, get_user_auth
from sclib.config import Config
from sclib.logmixin import LogMixin
from sclib.utils import *
from scsettings.common import *

import json
import logging
import re
import sys
import os
import inspect

import SplunkIt

import splunklib.results as results
import splunklib.client as client

class SplunkItIndex(LogMixin):
    def __init__(self, args):
        self.si = SplunkIt.SplunkIndex()
        self.input = args.get("input") 
        self.project = args.get("project")
        self.flow = args.get("flow")
        self.index = args.get("index")
        self.index = self.si.getIndex({"project" : self.project, "flow" : self.flow}) if self.index is None else self.index

    def is_valid_index(self):
        # This condition catches error from getIndex function
        if "ERROR" in self.index:
            abort(self.logger, self.index)

        # This condition catches a case where user passes an incorrect index name
        if not self.si.isValidIndex(self.index):
            abort(self.logger, "Index " + self.index + " does not exist")
        return True
        
    def get_index_name(self):
        if self.is_valid_index():
            return self.index

    def list_indexes(self):
        return self.si.listIndexes()

    def get_monitor_dir(self):
        if self.is_valid_index():
            mdir = self.si.getCacheDir({"index" : self.index})
            if os.path.isdir(mdir) and os.access(mdir, os.W_OK):
                return (mdir, True)
            else:
                self.logger.debug("Monitor directory not found for index {}. Using current work dir as cache dir.".format(self.index))
                # FIXME: log as a splunk event to setup a daily alert    
                # send_monitor_email(self.index, self.input)
                curdir = os.getcwd()
                mdir = curdir if os.path.isdir(curdir) and os.access(curdir, os.W_OK) else os.getenv("HOME")
                return (mdir, False)
    

class SplunkService(LogMixin):
    def __init__(self, **kwargs):
        if "host" not in kwargs:
            kwargs["host"] = HOST
        self.host = kwargs["host"]

        if ("rc" in kwargs):
            self.auth = get_splunk_cred_splunkrc(kwargs["rc"])
        else:            
            self.auth = get_user_auth()        

        if "username" not in kwargs:
            kwargs["username"] = self.auth[0]
        if "password" not in kwargs:
            kwargs["password"] = self.auth[1]

        try:
            self.service = client.connect(**kwargs)
        except Exception as e:            
            abort(self.logger, str(e))            

class SplunkSearch(SplunkService, LogMixin):
    def __init__(self, **kwargs):
        self.verbose = False
        self.search_args = Config().get_section('Search')
        SplunkService.__init__(self, **kwargs)

    def prepare_query(self, query):
        if not (query.startswith('search') or query.startswith("|")):
            query = 'search ' + query

        if self.verbose:
            self.logger.info("Search query: {}".format(query))
        self.logger.debug(query)

        return query

    def run_search(self, query, **kwargs):
        for k,v in self.search_args.iteritems():
            if k not in kwargs:
                kwargs[k] = v

        reader = self.service.jobs.export(query, **kwargs)
        search_events = str(reader).strip()

        return search_events
    
    def set_verbosity(self):
        self.verbose = True

class SplunkIndex(SplunkSearch, LogMixin):
    def __init__(self, index_name, **kwargs):
        self.metakey = get_metakey()

        if index_name is not None:
            self.index_name = index_name
            self.query_prefix = 'index=' + self.index_name + ' '
        else:
            self.index_name = ''
            self.query_prefix = ''

        SplunkSearch.__init__(self, **kwargs)

    def prepare_query(self, query, limit=1000):
        if self.index_name:
            query = self.query_prefix + query

        if not (query.startswith('search') or query.startswith("|")):
            query = 'search ' + query
        
        if int(limit) > 0:
            query += ' | head ' + str(limit)
        else:
            # default limit to 1000
            logger.debug(str(limit) + " limit must be greater than 0. Defaulting to limit 1000.")
            query += ' | head 1000'

        if self.verbose:
            self.logger.info("Search query: {}".format(query))
        self.logger.debug(query)

        return query

    def print_search_events_monitor_format(self, search_data, raw):
        for e in search_data:
            e = json.loads(e)
            if 'result' in e and e["result"]:
                if raw:
                    if '_raw' in e["result"]:
                        print(eval(json.dumps(e["result"]["_raw"])))
                else:
                    print(json.dumps(e["result"]))

    def search(self, query, limit, **kwargs):
        query = self.prepare_query(query, limit)

        if kwargs["output_mode"] == "monitor":
            kwargs["output_mode"] = "json"

        suffix = ' | table _raw, _time'

        search_events = self.run_search(query + suffix, **kwargs)
        search_data = search_events.split("\n")

        if self.output_mode == "monitor":
            self.print_search_events_monitor_format(search_data, True)
        elif self.output_mode == "json":
            print("[", end="")
            for idx,e in enumerate(search_data):                    
                e = json.loads(e)
                if 'result' in e and e["result"]:
                    if '_raw' in e["result"]:
                        if idx == len(search_data) - 1:
                            print("{}".format(json.dumps(e["result"]["_raw"])), end="")  
                        else:
                            print("{},".format(json.dumps(e["result"]["_raw"])), end="")  
            print("]") 
        else:
            print(search_events)

        return search_events

    def search_raw(self, query, **kwargs):
        if kwargs["output_mode"] == "monitor":
            kwargs["output_mode"] = "json"

        search_events = self.run_search(query, **kwargs)
        search_data = search_events.split("\n")

        if self.output_mode == "monitor":
            self.print_search_events_monitor_format(search_data, False)
        elif self.output_mode == "json":
            print("[", end="")
            for idx,e in enumerate(search_data):
                e = json.loads(e)
                if 'result' in e and e["result"]:
                    if idx == len(search_data) - 1:
                        print("{}".format(json.dumps(e["result"])), end="")
                    else:
                        print("{},".format(json.dumps(e["result"])), end="") 
            print("]")
        else:
            print(search_events)

        return search_events

    @staticmethod
    def is_locked(index_name):
        # check if the selected index is locked for injection
        logger = logging.getLogger(__name__)
        for i in get_index_blacklist():
            rex = re.compile('%s'%i)
            if rex.search(index_name):
                logger.error(index_name + " index is locked for data injection")
                return True
        return False

class SplunkEvent(SplunkIndex, LogMixin):
    def __init__(self, args, **kwargs):
        # output_mode monitor is not supported by Splunk Python SDK
        # so we need to store it in a class variable and still search
        # in json mode, we will do monitor formatting in SplunkIndex
        # search methods
        self.output_mode = args["output_mode"]

        if args["output_mode"] == "monitor":
            kwargs["output_mode"] = "json"     

        SplunkIndex.__init__(self, args["index"], **kwargs)

        self.source = args["source"]
        self.sourcetype = args["sourcetype"]
        self.verbose = False

        if "verbose" in kwargs:
            self.verbose = True

    def get_search_query(self, dict):
        meta = get_metadata()
        key = self.metakey
        search_args = {
            'source' : self.source,
            'sourcetype' : self.sourcetype,
            key+'.oduser' : meta[key]["oduser"],
            key+'.project' : meta[key]["project"]
        }
        dict.update(search_args)
        return ' '.join(['"%s"="%s"' % (key, value) for (key, value) in sorted(dict.items())])

    def select(self, query, raw=False, limit=1000, **kwargs):
        if raw:
            search_events = self.search_raw(query, **kwargs)
        else:
            search_events = self.search(query, limit, **kwargs)

        if self.verbose:
            self.logger.info("Events found: {}".format(len(search_events)))
        
    @staticmethod
    def insert(json_data, args):
        logger = logging.getLogger(__name__)
        si = SplunkItIndex(args)
        index = si.get_index_name()
        mdir, is_exists = si.get_monitor_dir()

        if SplunkIndex.is_locked(index):
            abort(logger, "Index " + index + " is locked for data injection")

        # Inject with file monitoring
        if "monitor" in args and args.get("monitor") is True: 
            if not is_exists:                
                abort(logger, "Monitor directory not found for index {}. Exiting!".format(index))
            input = args.get("input")
            if write_monitor_json(json_data, mdir, input):
                logger.info("File {} successfully copied to splunk monitor directory {} for index {}".format(input, mdir, index))
                return True
            else:
                abort(logger, "Failed to write file {} to the splunk monitor directory {} for index {}".format(input, mdir, index))                

        # Inject with HEC 
        params = {        
            'host' : args['hec_host'],
            'index' : index,
            'source' : args["source"],
            'sourcetype' : args["sourcetype"],
            'buffer_size' : 20,
            'cache_dir' : mdir,
            # disable additional metadata from SplunkIt
            'add_metadata' : 0
        }

        logger.debug("SplunkIt Params: " + json.dumps(params))    
        splunk_it = SplunkIt.ProjectEventCollector(params)

        if splunk_it is None:            
            abort(logger, "SplunkIt instantiation failed")
        try:
            splunk_it.info(json.dumps(json_data))
            logger.info("Data inserted into index " + index)
        except Exception as e:            
            abort(logger, str(e))

class SplunkKVStore(SplunkService, LogMixin):
    def __init__(self, args, **kwargs):
        self.app = DEFAULT_APP if args.app is None else args.app
        self.owner = DEFAULT_OWNER if args.owner is None else args.owner
        self.name = args.collection
        self.collection = None
        kwargs['app'] = self.app
        kwargs['owner'] = self.owner
        SplunkService.__init__(self, **kwargs)
        if self.exists():
            self.collection = self.service.kvstore[self.name]
        elif not args.list:
            self.logger.error("Invalid namespace (" + self.app + "/" + self.name + ")")
            raise ValueError

    def list_collections(self):
        names = []
        for collection in self.service.kvstore:
            names.append(collection.name)
        return names

    def exists(self):
        if self.name in self.list_collections():
            return True
        return False

    def query_by_id(self, id):
        result = {}
        if self.collection is not None:
            result = self.collection.data.query_by_id(id)
        return json.dumps(result, indent=1)

    def query(self, json_string=None):
        result = {}
        if self.collection is not None:
            if json_string is None:
                result = self.collection.data.query()
            else:                
                result = self.collection.data.query(query=json_string)
        return json.dumps(result, indent=1)

    def batch_insert(self, documents):
        # documents: list of dicts
        if self.is_valid_list_of_dicts(documents):  
            return self.collection.data.batch_save(*documents)
        else:
            self.logger.error("Invalid document for insert")
            raise ValueError

    def update(self, documents=None):
        # documents: json object
        if self.is_valid_list_of_dicts(documents):               
            # we have array of updates
            if self.is_valid_data(documents):
                results = []
                for entry in documents:
                    results.append(self.collection.data.update(entry['_key'], json.dumps(entry)))
                return results
            else:
                self.logger.error("Invalid document for update. Ensure you have _key in your documents.")
        else:
            # we have single document
            if '_key' in documents:
                return self.collection.data.update(documents['_key'], json.dumps(documents))
            else:
                self.logger.error("Invalid document for update. Ensure you have _key in your document.")
            raise ValueError

    # helper method for update_append function
    def update_append_entry(self, entry):
        self.logger.debug("ENTRY")
        self.logger.debug(entry)

        # be careful of strings with curly braces when using the str format method
        json_string = '{' + '"_key":"{}"'.format(entry['_key']) + '}'                                 
        updated_entries = self.collection.data.query(query=json_string)

        # there should always only be one entry per _key               
        updated_entry = updated_entries[0]

        for e_key in entry.keys():
            if (e_key not in updated_entry or updated_entry[e_key] != entry[e_key]):
                self.logger.debug("UPDATING {} : {} -> {} : {}".format(e_key, updated_entry.get(e_key, "empty"), e_key, entry[e_key]))
            
            updated_entry[e_key] = entry[e_key]                            
        
        self.logger.debug("UPDATED ENTRY")
        self.logger.debug(updated_entry)

        return updated_entry

    def update_append(self, documents=None):
        # documents: json object
        if self.is_valid_list_of_dicts(documents):               
            # we have array of updates
            if self.is_valid_data(documents):
                results = []
                for entry in documents:                 
                    updated_entry = self.update_append_entry(entry)

                    results.append(self.collection.data.update(entry['_key'], json.dumps(updated_entry)))

                return results
            else:
                self.logger.error("Invalid document for update. Ensure you have _key in your documents.")
        else:
            # we have single document
            if '_key' in documents:
                entry = documents
                
                updated_entry = self.update_append_entry(entry)

                return self.collection.data.update(entry['_key'], json.dumps(updated_entry))
            else:
                self.logger.error("Invalid document for update. Ensure you have _key in your document.")
            raise ValueError

    def delete(self, documents=None):
        # documents: json object
        if self.is_valid_list_of_dicts(documents):
            # we have array of deletes                
            results = []                    
            for entry in documents:
                results.append(self.collection.data.delete(json.dumps(entry)))
            return results
        else:
            # we have single document            
            if (documents is None):
                # for deleteall
                self.logger.info("Deleting all entries in collection.")
                return self.collection.data.delete()
            else:
                document = json.dumps(documents)
                return self.collection.data.delete(document)
        
    def is_valid_list_of_dicts(self, documents):
        # example: [{...},{...}, ...]
        if (documents is not None) and (not isinstance(documents, dict)) and is_list_of_dicts(documents):
            return True
        return False

    def is_valid_data(self, documents):
        # example: [{"_key" : "id1", ...},{"_key" : "id2", ...}, ...]
        if (documents is not None) and (not isinstance(documents, dict)) and is_list_of_dicts(documents) and all("_key" in elem for elem in documents):
            return True
        return False

class SplunkCSV(SplunkSearch, LogMixin):
    def __init__(self, app, csv_file_name, csv_content, csv_schema, **kwargs):
        SplunkSearch.__init__(self, **kwargs)        
        # check if app exists, namespace is important for CSV
        query = "| rest /services/apps/local | search title={} | stats count".format(app)

        csv_args = {}
        csv_args["app"] = app
        csv_args["preview"] = False
        csv_args["output_mode"] = 'json'

        result = (json.loads(self.run_search(query, **csv_args)))["result"]
        count = int(result["count"])

        if (count == 0):
            self.logger.error("{} is not a valid app".format(app))
            self.logger.info("Valid apps: {}".format(str(app_list)))
            raise ValueError

        self.app = app
        self.csv_file_name = csv_file_name
        self.csv_content = csv_content

        if len(csv_schema) > 1:
            self.logger.error("Invalid schema, should consist of single JSON object {}")
            raise ValueError

        self.csv_schema = csv_schema[0]

    def lint(self):
        patterns = {}

        for column in self.csv_schema:            
            patterns[column] = re.compile(self.csv_schema[column])
        
        for csv_obj in self.csv_content:
            if len(self.csv_schema) != len(csv_obj):                
                self.logger.error("Schema columns do not match CSV columns")
                raise ValueError

            for column in csv_obj:
                if not patterns[column].search(str(csv_obj[column])):
                    self.logger.error("CSV schema validation failed")
                    self.logger.error("Validation failed for column: {} using pattern: {} on value: {}".format(column, self.csv_schema[column], csv_obj[column]))
                    self.logger.error("Please fix CSV errors and re-run lookup monitor")
                    raise ValueError

        self.logger.info("CSV schema validation successful... copying file")

        return True

    def monitor(self):
        # FIXME: remove hardcoded monitor path. pull it  from splunk_index
        csv_monitor_path = CSV_MONITOR_BASE_PATH.format(self.app, os.path.basename(self.csv_file_name))

        try:
            self.lint()

            syscmd("rsync -avzh {} {}".format(self.csv_file_name, csv_monitor_path))
        except Exception as e:
            self.logger.error("Failed to copy CSV file " + str(e))
            raise Exception(str(e))

if __name__ == '__main__':
    pass
