
import test

class TestArith(test.CompileExecute):

    def test_func_call(self):
        self.compile_and_exec("""
            func main: int
            =; add 3 7

            func add: int -> int -> int
            = a b ; a + b
        """)
        assert self.register_value(6) == 10

    def test_func_call_func_param(self):
        self.compile_and_exec("""
            func main: int
            =; add (three) (seven)

            func add: int -> int -> int
            = a b ; a + b

            func three: int
            =; 3

            func seven: int
            =; 7
        """)
        assert self.register_value(6) == 10

