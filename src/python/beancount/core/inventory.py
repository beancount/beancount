import os
if os.getenv('XPOS'):
    from beancount.core.xinventory import *
else:
    from beancount.core.oinventory import *
