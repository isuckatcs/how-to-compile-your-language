import os
import lit.formats

config.name = "how-to-compile-your-language"
config.test_format = lit.formats.ShTest(False if os.name == 'nt' else True)

config.suffixes = ['.al']
