import test

class TestCond(test.CompileExecute):

    def test_normal_cond(self):
        self.compile_and_exec("""
            func main: int
            =; if 1 == 1 then 10 else 20
        """)
        self.assertResult(10)

    def test_normal_cond_neg(self):
        self.compile_and_exec("""
            func main: int
            =; if 1 == 2 then 10 else 20
        """)
        self.assertResult(20)

    def test_normal_cond_nested(self):
        self.compile_and_exec("""
            func main: int
            =; if 1 == 1 then if 1 == 1 then 40 else 10 else 20
        """)
        self.assertResult(40)
