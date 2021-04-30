"""auth.py
Credits: implementation borrowed from confluence_cli tool
"""
from __future__ import print_function

from sclib.config import Config
from scsettings.common import *

import getpass
import logging
import warnings
import platform

linux_distribution=platform.linux_distribution()

logger= logging.getLogger(__name__)

with warnings.catch_warnings():
    # ignore warning - not using mpz_powm_sec
    warnings.simplefilter("ignore")
    import keyring
    import platform
    import sys

    # FIXME: platform.linux_distribution is deprecated in python 3.6
    # instead use distro.linux_distribution
    linux_distribution = platform.linux_distribution();
    if (linux_distribution[1][0] == '6' and 
        (sys.version_info.major == 2 and sys.version_info.minor == 7 and sys.version_info.micro == 11)):
        keyring.set_keyring(keyring.backends.file.PlaintextKeyring())
    else:
        import keyrings.alt
        keyring.set_keyring(keyrings.alt.file.PlaintextKeyring())  

def get_login(username = None,refresh=False):    
    '''
    Get the password for username out of the keyring.
    '''
    if username is None:
        username = getpass.getuser()

    passwd = keyring.get_password(SPLUNKCLI, username)

    if refresh or passwd is None:
        passwd = getpass.getpass('Enter OD Password:')
        keyring.set_password(SPLUNKCLI, username, passwd)

    return (username, passwd)


def delete_passwd(username):
    keyring.delete_password(SPLUNKCLI, username)


def pw_change_prompt(username):
    global input
    try: input = raw_input
    except NameError: pass
    behavior = input('Modify Password [M] or Delete [D]: ')
    if (behavior == 'D' or behavior == 'd'):
        delete_passwd(username)
    elif (behavior == 'M' or behavior == 'm'):
        get_login(username, refresh=True)
    else:
        logger.error('Invalid response, not doing anything')
        raise

# extract and store mxd credentials from $HOME/.splunkrc file
# ${HOME}/.splunkrc file format    
# user splunk_username
# passwd splunk_password
def get_splunk_cred_splunkrc(rc): 
    auth_logger= logging.getLogger(__name__)

    try:
        splunkrc = {}
        
        with open(os.getenv('HOME') + '/' + rc) as f:
            content = f.readlines()
        
        content = filter(lambda x: not x.startswith('#'), [x.strip() for x in content])
        for line in content:
            (key,val) = line.split(" ");
            splunkrc[key] = val

        auth = (splunkrc['user'], splunkrc['passwd'])
    except Exception as e:
        abort(auth_logger, "Failed to read .splunkrc file {}.".format(str(e)))

    return auth

def get_user_auth():
    return get_login(Config().get_setting('User','username'))

def set_user(username):
    try:
        cfg = Config()
        cfg.set_setting('User','username', username)
        cfg.write_config(overwrite=True)
        logger.info('Username updated')
    except:
        logger.error('Username not updated')

def set_passwd():
    try:
        pw_change_prompt(Config().get_setting('User','username'))
        logger.info('Password updated')
    except:
        logger.error('Password not updated')

if __name__ == "__main__":
    pass
