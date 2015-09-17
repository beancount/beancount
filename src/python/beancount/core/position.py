import os
if os.getenv('XPOS'):
    from beancount.core.xposition import *
else:
    from beancount.core.oposition import *
