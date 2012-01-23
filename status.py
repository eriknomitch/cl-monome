#! /usr/bin/env python

import time, random
from monome import Monome, find_any_monome

host, port = find_any_monome()

m = Monome((host, port))
m.start()

#def mycallback(x, y, s):
#    m.led_set(x, y, s)
#
#m.grid_key = mycallback
#
#m.led_all(0)
#try:
#    while True:
#        for i in range(8):
#            m.led_row(0, i, random.randint(0,255))
#            time.sleep(1.0/20)
except KeyboardInterrupt:
    m.led_all(0)
    m.close()

