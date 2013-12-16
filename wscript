

def options(c):
    c.load('compiler_c')

def configure(c):
    c.load('compiler_c flex bison')
    c.env.LIBPATH_NEART = ['/usr/local/Cellar/flex/2.5.37/lib/']
    c.env.STLIB_NEART = ['fl']

def build(c):
    c.recurse('src')
