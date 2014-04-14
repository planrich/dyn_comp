import test

class TestArith(test.CompileExecute):

    def test_normal_cond(self):
        self.compile_and_exec("""
            func main: int
            =; if 1 == 1 then 10 else 20
        """)
        assert self.register_value(6) == 10

    def test_normal_cond_neg(self):
        self.compile_and_exec("""
            func main: int
            =; if 1 == 2 then 10 else 20
        """)
        assert self.register_value(6) == 20

    def test_normal_cond_nested(self):
        self.compile_and_exec("""
            func main: int
            =; if 1 == 1 then if 1 == 1 then 40 else 10 else 20
        """)
        assert self.register_value(6) == 40
