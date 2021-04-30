import logging
import logging.handlers
import os
import time

# https://stackoverflow.com/questions/15780151/how-to-use-python-logging-in-multiple-modules
# https://stackoverflow.com/questions/29069655/python-logging-with-a-common-logger-class-mixin-and-class-inheritance

# handles the case where first user creates the log file and subsequent 
# user write attempts fail. This makes sure the file is group writable.
class GroupWriteRotatingFileHandler(logging.handlers.RotatingFileHandler):    

    def __init__(self, *args, **kwargs):
        logging.handlers.RotatingFileHandler.__init__(self, *args, **kwargs)

        self.num_error_retries = 0
        self.max_error_retries = 3
        self.error_occurred = False

    def _open(self):
        prevumask=os.umask(0o002)
        rtv=logging.handlers.RotatingFileHandler._open(self)
        os.umask(prevumask)
        return rtv


    def handleError(self, record):
        self.error_occurred = True
        # this is to handle the case of other processes rolling over files while this process is in the same action
        if self.num_error_retries < self.max_error_retries:
            self.num_error_retries += 1
            logging.FileHandler.emit(self, record)

        # past run was successful, or number of tries was maxed out
        self.num_error_retries = 0

    def emit(self, record):
        if self.error_occurred:
            #if an error occurred it will happen again, skip log roll over check/action
            logging.FileHandler.emit(self, record)
        else:
            logging.handlers.RotatingFileHandler.emit(self, record)


class LogMixin(object):
    @property
    def logger(self):
        name = '.'.join(['splunk_capture',__name__, self.__class__.__name__])
        return logging.getLogger(name)

# Assign the custom handler to the logging handlers
logging.handlers.GroupWriteRotatingFileHandler = GroupWriteRotatingFileHandler

if __name__ == '__main__':
    pass
