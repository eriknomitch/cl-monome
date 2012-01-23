#! /usr/bin/env python

import time, random
from monome import Monome, find_any_monome

host, port = find_any_monome()

m = Monome((host, port))
m.start()

#gdef mycallback(x, y, s):
#g    m.led_set(x, y, s)
#g
#gm.grid_key = mycallback
#g
#gm.led_all(0)
#gtry:
#g    while True:
#g        for i in range(8):
#g            m.led_row(0, i, random.randint(0,255))
#g            time.sleep(1.0/20)
#gexcept KeyboardInterrupt:
#g    m.led_all(0)
#g    m.close()
#g
