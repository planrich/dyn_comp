
import test

class TestArith(test.CompileExecute):

    def test_func_call(self):
        self.compile_and_exec("""
            func main: int
            =; loopTo10 0

            func loopTo10: int -> int
            = a ; if a == 10 then 10 else loopTo10 (1 + a)
        """)
        assert self.register_value(6) == 10
