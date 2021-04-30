import os
from multiprocessing import Process

def run_command():
    os.system("../splunk_capture event --index playground1 --input ../../test_data/MOCK_DATA.json")

if __name__ == '__main__':
    MAX_PROCESSES = 8
    for i in range(0, MAX_PROCESSES):
        print("Running Command")
        p = Process(target=run_command)
        p.start()
