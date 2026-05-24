import os
import shutil
import lit.formats

config.name = "how-to-compile-your-language"
config.test_format = lit.formats.ShTest(False if os.name == 'nt' else True)

config.suffixes = ['.yl']

config.substitutions += [
    ("%valgrind", "valgrind --error-exitcode=1" if shutil.which("valgrind") else "")
]
