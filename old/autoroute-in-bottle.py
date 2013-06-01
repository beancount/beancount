# FIXME: We should offer an 'autoroute' patch to Bottle.py.


# print(bottle.url('balsheet', no=10))
# print(app.get_url('balsheet', no=11))

# Failed attempts at modifying the decorator so that it inserts named route with
# the name of the wrapped callback automatically. This is impossible; you need
# to modify bottle itself.

# def route(*args, **kw):
#     def named_route(fun):
#         print('XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX', fun.__name__)
#         kw['name'] = fun.__name__
#         return bottle.route(*args, **kw)
#     return named_route

# def route(*args, **kw):
#     wrapper = bottle.route(*args, **kw)
#     def named_route(fun):
#         kw['name'] = fun.__name__
#         return wrapper(fun)
#     return named_route

# def autoroute(*args, **kw):
#     """Invoke a modified decorator that will have the name of the wrapped function
#     as the name of the route."""
#     wrapper = bottle.route(*args, **kw)
#     def named_wrapper(callback):
#         name = callback.__name__
#         print(name)
#         return wrapper(callback)
#     return named_wrapper

#FIXME: I want to be able to render links with a global index of symbols, not as links, just like I did in my own thing.
# This makes everything nicer...
# def route(path, *args):
#     def decorator(fun):
#         fun = route(path, *args)




# def render(pagetitle, contents, **kw):
#     """Render the title and contents in our standard template."""
#     output = bottle.template(template,
#                              pagetitle=pagetitle)
#     return output.format(navigation="", contents=dedent(contents))
