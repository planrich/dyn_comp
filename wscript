
import sys
import subprocess

def options(c):
    c.load('compiler_c')
    # we are in debug mode all the time
    c.add_option('--debug', action='store_true', default=True, help='compile in debug mode')
    c.add_option('--test', action='store_true', default=False, help='build the test suit')
    c.add_option('--wall', action='store_true', default=False, help='show all warnings generated from cc')


def configure(c):
    c.load('compiler_c flex bison')
    c.env.BISONFLAGS = ['-d','--report=solved']
    if sys.platform.startswith("darwin"):
        c.env.LIBPATH += ['/usr/local/Cellar/flex/2.5.37/lib/']

    c.env.STLIB += ['fl']

    if c.options.debug:
        c.env.CFLAGS += ['-g']
    if c.options.wall:
        c.env.CFLAGS += ['-Wall']
    try:
        scmhash = subprocess.check_output(
            "git log --pretty=format:%H | tail -1", universal_newlines=True, shell=True)[0:8]
    except Exception as e:
        scmhash = 'nohash'
    major = 0
    minor = 1
    bug = 0

    c.define('NEART_VERSION', "%d.%d.%d" % (major,minor,bug))
    c.define('NEART_SCM_HASH', scmhash)
    if c.options.debug:
        c.define('NEART_VISUAL', 1)
    c.write_config_header('src/config.h')

    c.recurse('src')
    if c.options.test:
        c.recurse('test')

def build(c):
    c.objects( target = 'klib', source = 'third/klib/kstring.c', includes = 'third/klib' )
    c.recurse('src')
    if c.options.test:
        c.recurse('test')
