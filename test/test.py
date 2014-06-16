
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
            subprocess.call(['./build/src/nvm', '-v', 'code.nc'], stdout=exec_log)
        with open('tmp/exec.log', 'r') as exec_log:
            self.output = exec_log.read()

        # jit
        with open('tmp/exec_jit.log', 'w') as exec_log:
            subprocess.call(['./build/src/nvm', '-vj', 'code.nc'], stdout=exec_log)
        with open('tmp/exec_jit.log', 'r') as exec_log:
            self.jit_output = exec_log.read()

    def register_value(self, register, output=None):
        if output is None:
            output = self.output
        return int(re.search(r'register {} == (\d+)'.format(register), output).group(1)) # and \
            #int(re.search(r'register {} == (\d+)'.format(register), self.jit_output).group(1))
    def assertResult(self, result): 
        self.assertEquals(self.register_value(6, output=self.output), result)
        self.assertEquals(self.register_value(6, output=self.jit_output), result)
