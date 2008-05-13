prioritized = ['inbox', 'lists']

def unfair_cmp(x, y):
   for prefix in prioritized:
       xsw = x.startswith(prefix)
       ysw = y.startswith(prefix)
       if xsw and ysw:
          return cmp(x, y)
       elif xsw:
          return -1
       elif ysw:
          return +1
   return cmp(x, y)
