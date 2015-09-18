import os
if os.getenv('XINV'):
    from beancount.core.xinventory import *
else:
    from beancount.core.oinventory import *
