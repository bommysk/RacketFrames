"""splunk_config.py
Credits: implementation borrowed from spyder/config.py
"""

from __future__ import print_function

from sclib.logmixin import LogMixin
import scsettings.common as const

try:
    # python 2.7.x
    import ConfigParser
except ImportError:
    # python 3.x
    import configparser as ConfigParser

import logging
import os
import stat
import sys

# python singleton alternative to share state across instances
# http://www.aleax.it/5ep.html
# http://thepolyscope.com/en/python-singleton-or-borg/
class Borg(object):
    __shared_state = {}
    def __init__(self):
        self.__dict__ = self.__shared_state

# Add a method to write out the config state with values commented out -- this seeds the user's config file
class ConfigParserComments(ConfigParser.RawConfigParser):
    def write_commented(self, fp):
        """Write an .ini-format representation of the configuration state with values commented out"""
        if self._defaults:
            fp.write("[{}]\n".format(ConfigParser.DEFAULTSECT))
            for (key, value) in self._defaults.items():
                fp.write("; {} = {}\n".format(key, str(value).replace('\n', '\n\t')))
            fp.write("\n")
        for section in self._sections:
            fp.write("[{}]\n".format(section))
            for (key, value) in self._sections[section].items():
                if key == "__name__":
                    continue
                if (value is not None) or (self._optcre == self.OPTCRE):
                    key = " = ".join((key, str(value).replace('\n', '\n\t')))
                fp.write("; {}\n".format(key))
            fp.write("\n")


# Config class
class Config(Borg, LogMixin):
    def __init__(self, config_file=None):
        Borg.__init__(self)
        if not config_file:
            self.config_file = const.USER_CONFIG_FILE
        else:
            self.config_file = config_file
        self.config_settings = const.USER_CONFIG
        self.load_configs()

    def load_configs(self, cl_cfg=None):
        user_cfg = self.read_config(self.config_file)
        self.merge_configs(user_cfg, cl_cfg)

    def merge_configs(self, user_cfg, cl_cfg):
        if not user_cfg:
            self.logger.info('No user config file found -- creating "{}"'.format(self.config_file))
            self.write_config()
        for k in self.config_settings:
            if k not in user_cfg:
                if user_cfg:
                    self.logger.info('Config "{}" not found in user config file'.format(k))
                continue
            # simple string to boolean or int conversion
            self.config_settings[k].update((x, self.str_convert(y)) for x, y in user_cfg[k].items())

        # command line config trumps all but doesn't contain sections for terseness
        if cl_cfg:
            for section in self.config_settings:
                for k in self.config_settings[section]:
                    if k in cl_cfg:
                        self.config_settings[section][k] = self.str_convert(cl_cfg.pop(k))
            for k in cl_cfg:
                self.logger.error('Config "{}" not found'.format(k))

        # self.logger.debug('Merged Config Settings: {}'.format(self.config_settings))

    def get_section(self, section_name):
        return self.config_settings[section_name]

    def get_sections(self):
        return self.config_settings.keys()

    def get_all_keys(self, sections=None):
        all_keys = set()
        if not sections:
            sections = self.get_sections()
        for s in sections:
            skeys = [sk for sk in self.config_settings[s].keys() if not sk.startswith('__')]
            self.logger.debug('skeys = {}'.format(skeys))
            all_keys |= set(skeys)
        return sorted(list(all_keys))

    def get_setting(self, section, name, **kwargs):
        if 'defaultval' not in kwargs:
            return self.config_settings[section][name]
        else:
            try:
                return self.config_settings[section][name]
            except KeyError:
                return kwargs['defaultval']

    def set_setting(self, section, name, value):
        self.config_settings[section][name] = self.str_convert(value)

    @staticmethod
    def str_convert(s):
        try:
            if s.lower() == 'true':
                return True
            elif s.lower() == 'false':
                return False
        except AttributeError:
            return s
        try:
            res = int(s)
            return res
        except ValueError:
            return s

    def read_config(self, filen=None):
        if not filen:
            filen = self.config_file
        config = ConfigParser.ConfigParser()
        config.read(filen)
        return config._sections

    def write_config(self, filen=None, overwrite=False):
        if not filen:
            filen = self.config_file
        if os.path.exists(filen) and not overwrite:
            self.logger.debug('Config file "%s" exists, skipping generation', filen)
            return

        #config = ConfigParserComments()
        config = ConfigParser.ConfigParser()

        fdir = os.path.dirname(filen)
        if not os.path.exists(fdir):
            os.makedirs(fdir)

        # When adding sections or items, add them in the reverse order of
        # how you want them to be displayed in the actual file.
        for section in self.config_settings:
            config.add_section(section)
            for k, v in self.config_settings[section].items():
                config.set(section, k, v)

        # Write configuration file
        with open(filen, 'w') as configfile:
            #config.write_commented(configfile)
            config.write(configfile)

        # set file permissions to owner readonly
        os.chmod(filen, stat.S_IRUSR | stat.S_IWUSR)

if __name__ == "__main__":
    # c = Config()
    # print(c.config_file)
    # print(c.config_settings)
    # c.write_config()
    pass
