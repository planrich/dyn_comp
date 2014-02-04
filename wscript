
import subprocess

def options(c):
    c.load('compiler_c')

def configure(c):
    c.load('compiler_c flex bison')
    c.env.LIBPATH_NEART = ['/usr/local/Cellar/flex/2.5.37/lib/']
    c.env.STLIB_NEART = ['fl']
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
    c.write_config_header('src/config.h')

def build(c):
    c.recurse('src')
