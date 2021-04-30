HOST = 'insights.csg.apple.com'
HEC_HOST = 'https://splunk-segue-fwd.csg.apple.com:8088'
HEC_TOKEN = 'default'
SPLUNKCLI = 'splunk-cli-prod'
LOGGER_CONFIG = 'logging.conf'
EVENTS_URL = "https://" + HOST + '/app/search/search/?q=search+metadata__.oduser::{username}+metadata__.timestamp::{unix_timestamp}'
MONITOR_DIRECTORY_TEMPLATE = "/org/seg/services/insights/site/{}/monitor/{}"
MONITOR_USER = "socpuppet"
PHANTOMJS_SCRIPT_PATH = '/org/seg/tools/eda/apple/splunk/splunk_capture/'
PHANTOMJS_BIN = '/org/seg/services/vhost/mxd.csg.apple.com/content/gizmos/phantomJS/phantomjs'
DEFAULT_OWNER = 'nobody'
DEFAULT_APP = 'search'
CSV_MONITOR_BASE_PATH = '/site/scv/org-seg-services-insights-site-scv-monitor/csv/{}/lookups/{}'
