import os
if os.getenv('OLDPOS'):
    from beancount.core.oposition import *
else:
    from beancount.core.xposition import *
