

import test

class TestLoop(test.CompileExecute):

    def test_func_call(self):
        self.compile_and_exec("""
            func main: int
            =; sum 10


            func sum: int -> int
            = a ; if a == 0 then 0
                  else (sum (a-1)) + a
        """)
        self.assertResult(sum(range(0,11)))
