
import re
import subprocess
import unittest

class CompileExecute(unittest.TestCase):

    def compile_and_exec(self, code):
        with open('tmp/tmp.ne', 'w') as tmp:
            tmp.write("unit test\n")
            tmp.write(code)

        with open('tmp/compile.log', 'w') as compile_log:
            subprocess.check_call('./build/src/neartc < tmp/tmp.ne', stdout=compile_log, shell=True)

        with open('tmp/exec.log', 'w') as exec_log:
            subprocess.call(['./build/src/nvm', 'code.nc'], stdout=exec_log)
        with open('tmp/exec.log', 'r') as exec_log:
            self.output = exec_log.read()

    def register_value(self, register):
        return int(re.search(r'register {} == (\d+)'.format(register), self.output).group(1))


if __name__ == "__main__":
    unittest.run()
