"""utils.py
Helper methods for use in other modules
"""

from __future__ import print_function
from socket import gethostname

import copy
import logging
import json
import os
import re
import subprocess
import sys
import time
import stat
from string import Template
import smtplib
from email import encoders
from email.mime.application import MIMEApplication
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
from email.utils import COMMASPACE, formatdate

import SplunkIt

logger= logging.getLogger(__name__)
UNIX_TIMESTAMP = int(time.time())

# unable to get around Python prevention of double import
SPLUNK_ADMIN_EMAIL = 'Segue_Splunk_Admin@group.apple.com'
SPLUNK_ADMIN_NAME = 'Splunk Admins'

def abort(logger, msg=None):
    if msg is not None and len(msg):
        if isinstance(msg,list):
            for i in msg:
                logger.error(i)
        else:
            logger.error(msg)
            sys.exit(1)

def ed(key,str):
    key = key.decode('base64').strip()
    return "".join([chr(ord(a) ^ ord(b)) for a,b in zip(str,key)])

def filter_dict_by_keys(dict, list_of_keys):
    # takes a dictionary and
    # an array of string (or regex) patterns
    # and extract kv pairs of keys matching the patterns
    result = {}
    for i in list_of_keys:
        pattern = re.compile(i)
        for k in dict:
            if k not in result and pattern.match(k):
                result[k] = dict[k]
    return result

def hex_escape(s):
    import string
    printable = string.ascii_letters + string.digits + string.punctuation + ' '
    return ''.join(c if c in printable else r'\x{0:02x}'.format(ord(c)) for c in s)

def get_curtime():
    UNIX_TIMESTAMP = int(time.time())
    return time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(UNIX_TIMESTAMP))

def get_timezone():
    return time.tzname[time.daylight]

def gen_uuid():
    import uuid
    uid_str = uuid.uuid1()
    uid_list = str(uid_str).split('-')
    return uid_list[2] + uid_list[1] + uid_list[0] + ''.join(uid_list[3:])

def get_od_user():
    try:
        return syscmd('seg-info -OD_of_user ' + os.getenv('USER')).rstrip()
    except:
      logging.warning("OD user extraction failed")

    #default to environment variable user if it exists
    env_user = os.getenv('USER')

    if env_user:
        return env_user
    
    return "UNKNOWN USER"

def get_email_user():
    try:
        return syscmd('seg-info -email_of_user ' + os.getenv('USER')).rstrip()
    except:
      logging.warning("OD user email extraction failed")

    #default to environment variable user if it exists
    env_user = os.getenv('USER')

    if env_user:
        return env_user + "@csg.apple.com"
    
    return "UNKNOWN USER EMAIL"



def get_site():
    try:
        return syscmd('seg-info -get_site').rstrip()
    except:
      	logging.warning("Site name extraction failed")

    env_site = os.getenv('APPLE_SITE')

    if env_site:
        return env_site

    return "UNKNOWN SITE"




def get_project_info(project, field):
    try:
        return syscmd('proj-info -project {} -json -get {}'.format(project, field)).rstrip().strip('\"')
    except:
        logging.warning("Project info extraction failed")

def is_list_of_strings(l):
    return bool(l) and isinstance(l, list) and all(isinstance(elem, basestring) for elem in l)

def is_list_of_dicts(l):
    return bool(l) and isinstance(l, list) and all((isinstance(elem, dict) and len(elem)) for elem in l)

def json2dict(json_str, extra=None):
    from scsettings.common import get_metakey
    k = get_metakey()
    try:
        json_obj = json.loads(json_str, encoding='utf-8')
        if isinstance(json_obj, dict):
            l = []
            l.append(json_obj)
            json_obj = l
        if not is_list_of_dicts(json_obj):
            raise ValueError(json_str)

        ary_len = len(json_obj)
        i = 0
        while i < ary_len:
            if extra is not None and len(extra)>0:
                d = copy.deepcopy(extra)
                json_obj[i].update(d)
            if k in json_obj[i]:
                json_obj[i][k].update({
                    "uuid" : gen_uuid(),
                    "updated" : get_curtime()
                })
            i += 1
        return json_obj
    except ValueError as e:
        logger.error("Invalid JSON string - " + json_str)
        raise ValueError(str(e))
    except Exception as e:        
        abort(logger, str(e))

def syscmd(cmd):
    try:
        return subprocess.check_output(cmd, stderr=subprocess.STDOUT, shell=True)
    except subprocess.CalledProcessError as e:
        logging.error(e.output)

def make_directory(path, user_name, group_name):
    try:
        if (not os.path.isdir(path)):
            os.mkdir(path)
            # give owner and group write permission
            os.chmod(path, stat.S_IRWXU | stat.S_IRWXG | stat.S_IROTH)
            
            import pwd
            import grp

            uid = pwd.getpwnam(user_name).pw_uid
            gid = grp.getgrnam(group_name).gr_gid
            os.chown(path, uid, gid)
        else:
            logger.debug("The directory {} already exists, not creating.".format(path))
            return False
    except OSError as e:  
        abort(logger, "Creation of the directory {} failed".format(path))
    else:  
        logger.info("Successfully created the directory {}".format(path))
        return True

def copy_file(source, dest):
    try:
        logger.info("cp {} {}".format(source, dest))
        return syscmd("cp {} {}".format(source, dest))
    except:
        logging.warning("Copying the file failed")

def is_valid_monitor_json(path):
    with open(path) as infile:
        for line in infile:
            try:
                json.loads(line)
            except ValueError as e:
                logger.error(str(e))
                return False
    return True

def write_monitor_json(data, dest_dir, input_file):
    try:
        output_file = dest_dir + "/" + gethostname() + "." + str(UNIX_TIMESTAMP) + "." + str(os.getpid()) + "." + os.path.basename(input_file)
        with open(output_file, 'w') as outfile:
            # write json in minified format
            if type(data) == list:
                for line in data:
                    outfile.write(json.dumps(line, separators=(',',':')))
                    outfile.write("\n")
            else:
                outfile.write(json.dumps(data, separators=(',',':')))
    except IOError as e: 
        logger.error(str(e))
        return False
    except ValueError as e:
        logger.error(str(e))
        return False
    except TypeError as e:
        logger.error(str(e))
        return False
    except:        
        abort(logger, "Unexpected error: {}".format(sys.exc_info()[0]))
    return True

# message_template must be a valid file
def read_template(message_template):
    try:
        with open(message_template, 'r') as template_file:
            template_content = template_file.read()
    except:
        template_content = message_template

    return Template(template_content)

# Example:
# send_email(me, 'hemanth@apple.com, ader@apple.com, thol@apple.com', 'subject', 'message template', {template values}, files=['my.png'], server='relay.apple.com')
def send_email(send_from, send_to, cc, bcc, subject, message_template, message_values={}, files=None, server="relay.apple.com", dformat=None):
    plain_text = MIMEText('plain text', 'plain')
    html_text = MIMEText('html text', 'html')

    if not dformat:
        dformat = 'plain'

        message_template = read_template(message_template)
        text = message_template.substitute(message_values)

        plain_text = MIMEText(text, dformat)
    else:
        text = message_template

        html_text = MIMEText(text.decode('utf-8').encode('utf-8'), 'html','utf-8')

    # Create message container - the correct MIME type is multipart/alternative.
    msg = MIMEMultipart("alternative", None, [plain_text, html_text])
    
    if dformat == 'plain':
        msg.attach(plain_text)
    else:
        msg.attach(html_text)

    msg['From'] = send_from
    msg['To'] = send_to 
    msg['Cc'] = cc
    msg['Date'] = formatdate(localtime=True)
    msg['Subject'] = subject

    logger.debug('files = "{}"'.format(files))
    logger.debug('server = "{}"'.format(server))
    
    if not files:
        files = []
    
    for f in files:
        with open(f, "rb") as fil:
            part = MIMEApplication(fil.read(), Name=os.path.basename(f))
            logger.debug('attachment; filename="%s"' % os.path.basename(f))
            part['Content-Disposition'] = 'attachment; filename="%s"' % os.path.basename(f)
            msg.attach(part)

    try:
        smtp = smtplib.SMTP(server)
        send_to_list = send_to.split(",")
    
        if (not cc is None):
           send_to_list += cc.split(",")

        if (not bcc is None):
            send_to_list += bcc.split(",")

        smtp.sendmail(send_from, send_to_list, msg.as_string())
        smtp.close()
    except Exception as e:        
        abort(logger, 'Email send error: \n{}'.format(str(e)))


def send_monitor_email(index_name, input_path, to_name=SPLUNK_ADMIN_NAME, to_email=SPLUNK_ADMIN_EMAIL):
    subject = "New Monitor Directory Request for index - {}".format(index_name)
    template = os.path.abspath(os.path.join(os.path.dirname(__file__), '..')) + '/static/monitor_message.txt'
    notify_email = get_email_user()
    msg_values = {
        'TO': to_name, 
        'FROM': get_od_user(),
        'INDEX_NAME': index_name,
        'INPUT_FILE': os.path.abspath(input_path),
        'NOTIFY': notify_email
    }
    send_email(notify_email, to_email, None, None, subject, template, msg_values)

def html_prepender(filename, html, html_filename):
    with open(filename, 'r') as f:
        content = f.read()
        f.seek(0, 0)
        with open(html_filename, 'w') as html_f:
            html_f.write(html.rstrip('\r\n') + '\n' + content)


if __name__ == '__main__':
    pass
