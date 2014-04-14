import test

class TestArith(test.CompileExecute):

    def test_normal_arith(self):
        self.compile_and_exec("""
            func main: int
            =; 1 + 2
        """)
        assert self.register_value(6) == 3

    def test_normal_arith_overflow(self):
        a = 2**32
        b = 1
        self.compile_and_exec("""
            func main: int
            =; {0} + {1}
        """.format(a, b))
        assert self.register_value(6) == 1
