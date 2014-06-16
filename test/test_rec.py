

import test

class TestLoop(test.CompileExecute):

    def test_func_call(self):
        for i in range(0,120):
            self.compile_and_exec("""
                func main: int
                =; sum {}


                func sum: int -> int
                = a ; if a == 0 then 0
                      else (sum (a-1)) + a
            """.format(i))
            self.assertResult(sum(range(0,i+1)))
