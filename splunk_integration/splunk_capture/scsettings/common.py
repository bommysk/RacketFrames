import os

from socket import gethostname
from sclib.utils import *
from scsettings.prod import *

# Override settings from DEV
if os.getenv('SPLUNK_CLI_DEV') is not None:
    from scsettings.dev import *

logger_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '.')) + '/'

# Override logger for alternate version
if os.getenv('SPLUNK_CAPTURE_VERSION') is not None:
    logger_conf = 'logging-'+os.getenv('SPLUNK_CAPTURE_VERSION')+'.conf'
    if os.path.isfile(logger_path + logger_conf): 
        LOGGER_CONFIG = logger_conf

LOGGING_CONFIG_FILE = logger_path + LOGGER_CONFIG

LOCALHOST = gethostname()
PORT = 8089
HEC_PORT = 8088
SOURCETYPE = '_json'
ED_KEY = '/qZ2KkK+T6b3oH8xIsvjJIcTdJiGCVQjiKN0OgAZFI2Y9ttoWcH6hq9DuhEUs18xgH0='
DEFAULT_OWNER = "nobody"
DEFAULT_APP = "search"
SEARCH_LIMIT = 1000

METADATA_KEY = "metadata__"
METADATA = {
    'user' : os.getenv('USER'),
    'oduser' : get_od_user(),
    'host': LOCALHOST,
    'project' : os.getenv('PROJECT'),
    'site' : get_site(),
    'created' : get_curtime(),
    'timestamp' : UNIX_TIMESTAMP,
    'timezone' : get_timezone()
}

USER_CONFIG_FILE = os.path.join(os.getenv('HOME'), '.'+SPLUNKCLI, SPLUNKCLI+'.conf')
USER_CONFIG = {
    'User' : {
        'username' : get_od_user()
    },
    'Splunk' : {
        'host' : HOST
    },
    'Search' : {
        'earliest_time' : '-1d',
        'latest_time' : 'now',
        'search_mode' : 'normal'
    },
    'General' : {
        'message_level' : 'Info'
    }
}

INDEX_EXCLUDE = [
    '^mxd_',
    '^_'
]

def get_index_blacklist():
    return INDEX_EXCLUDE

def get_metakey():
    return METADATA_KEY

def get_metadata():
    return { METADATA_KEY : METADATA }

def get_user_config_file():
    return USER_CONFIG_FILE

def get_user_config():
    return USER_CONFIG

def get_delete_login():
    return [SYSUSR, SYSKEY]

if __name__ == '__main__':
    pass
