
import test

class TestFunc(test.CompileExecute):

    def test_func_call(self):
        self.compile_and_exec("""
            func main: int
            =; add 3 7

            func add: int -> int -> int
            = a b ; a + b
        """)
        self.assertResult(10)

    def test_func_call_func_param(self):
        self.compile_and_exec("""
            func main: int
            =; add three seven

            func add: int -> int -> int
            = a b ; a + b

            func three: int
            =; 3

            func seven: int
            =; 7
        """)
        self.assertResult(10)


    def test_func_call_register_overlap(self):
        self.compile_and_exec("""
            func main: int
            =; add (add 1 1) (seven)

            func add: int -> int -> int
            = a b ; a + b

            func seven: int
            =; 7
        """)
        self.assertResult(9)
