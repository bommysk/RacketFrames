"""
Generic file reader class to parse various file formats
Supported formats: CSV, JSON, XLSX, MXD
"""

from __future__ import print_function

import inspect
import json
import logging
import os
import pandas as pd
import re
import sys

from sclib.logmixin import LogMixin
from sclib.utils import json2dict, syscmd, abort

class FileReader(LogMixin):
    """
    File reader base class
    """
    def __init__(self, filepath):
        self.filepath = filepath
        self.basename = os.path.basename(filepath)
        # self.extension = os.path.splitext(filepath)[1].lower().lstrip('.')

    def set_attr(self, allowed_keys, **kwargs):
        self.__dict__.update((k, v) for k, v in kwargs.iteritems() if k in allowed_keys)

    def parse_file(self, **kwargs):
        # https://stackoverflow.com/questions/4382945/abstract-methods-in-python
        raise NotImplementedError(inspect.stack()[0][3] + " not implemented")

    def validate_file(self):
        raise NotImplementedError()

    def df_to_json(self, df, extra=None, **kwargs):
        return json2dict(df.to_json(orient='records', force_ascii=False), extra)


class CSVFileReader(FileReader, LogMixin):
    def __init__(self, filepath):
        FileReader.__init__(self, filepath)
        # self.set_attr([''], **kwargs)

    def parse_file(self, extra=None, **kwargs):
        try:
            df = pd.read_csv(self.filepath, skip_blank_lines=True, encoding='utf-8', **kwargs).rename(columns=lambda x: x.strip())
            return self.df_to_json(df, extra, **kwargs)
        except Exception as e:
            abort(self.logger, "Failed to read CSV file - {}".format(str(e)))

class XLSFileReader(FileReader, LogMixin):
    def __init__(self, filepath):
        # call super class constructor
        FileReader.__init__(self, filepath)

    def parse_file(self, sheetname=None, extra=None, **kwargs):
        try:
            # instantiate pandas ExcelFile
            xl = pd.ExcelFile(self.filepath)

            # if sheetname is not provided, use the first sheet in excel
            if sheetname is None:
                sheetname =  xl.sheet_names[0]

            # call read excel in unicode
            df = pd.read_excel(xl, sheetname, encoding='utf-8', **kwargs)

            # replace whitespace in each cell value with a space
            # intent: change multi line values to a single line
            for col in list(df.columns.values):
                df[col].replace(regex=True, inplace=True, to_replace=r'\s+', value=r' ')

            # change header names to lowercase and replace spaces with underscores
            df.columns = df.columns.str.lower().str.replace(' ', '_')
            return self.df_to_json(df, extra, **kwargs)
        except Exception as e:            
            abort(self.logger, "Failed to read XLS file - {}".format(str(e)))

class JSONFileReader(FileReader, LogMixin):
    def __init__(self, filepath):
        FileReader.__init__(self, filepath)

    def parse_file(self, extra=None, **kwargs):
        json_data = None
        try:
            json_data = json.load(open(self.filepath))
        except Exception as e:
            abort(self.logger, "Failed to read JSON file - {}".format(str(e)))
        if json_data is not None:
            return json2dict(json.dumps(json_data), extra)

    def validate_file(self):
        # check if the format is list of objects
        pass

class MXDFileReader(FileReader, LogMixin):
    def __init__(self, filepath, metadata=None):
        FileReader.__init__(self, filepath)

    def parse_file(self, extra=None, **kwargs):
    	try:
            dict = {}
            p = re.compile('#MX#\s*(\S+)\s*\|\s*(.*)')
            with open(self.filepath) as f:
                content = f.readlines()
                for line in [x.strip() for x in content]:
                    match = p.findall(line)
                    if match and match[0] :
                        dict[match[0][0]] = match[0][1]
            if extra is not None:
                dict.update(extra)

            dict['SYS/MXD_FILE'] = syscmd("realpath {}".format(self.filepath)).strip()

    	    return json2dict(json.dumps(dict, ensure_ascii=False), extra)
        except Exception as e:
            abort(self.logger, "Failed to read MXD file - {}".format(str(e)))

    def validate_file(self):
        pass

class FileReaderFactory(LogMixin):
    """
    File reader factory class
    """
    VALID_FILE_TYPES = ['csv', 'json', 'mxd', 'xlsx']
    def __init__(self, filepath, filetype=None):
        self.filepath = filepath
        self.extension = os.path.splitext(filepath)[1].lower().lstrip('.')
        if (filetype is not None):
            self.extension = filetype
        if self.extension not in self.VALID_FILE_TYPES:            
            abort(self.logger, "Invalid file. Allowed file types: {}".format(",".join(self.VALID_FILE_TYPES)))

    def get_reader(self):
        return {
            'csv': CSVFileReader,
            'mxd': MXDFileReader,
            'xlsx' : XLSFileReader,
            'json' : JSONFileReader
        }.get(self.extension, FileReader)

    def parse_file(self, **kwargs):
        return self.get_reader()(self.filepath).parse_file(**kwargs)


if __name__ == '__main__':
    # print(FileReaderFactory('test.xlsx').parse_file())
    # print(FileReaderFactory('test.csv').parse_file())
    # print(FileReaderFactory('test.json').parse_file())
    # print(FileReaderFactory('test.mxd').parse_file())
    pass
