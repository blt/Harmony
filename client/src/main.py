import sys, pygame
from pygame import FULLSCREEN, DOUBLEBUF, RESIZABLE
from pygame import mouse
from pygame.locals import *
from star import Star
from creator import Creator
from planet import Planet
from struct import pack, unpack
#from planet import Planet

ADD_STAR   = int('00', 2)
ADD_PLANET = int('01', 2)
DEL_STAR   = int('11', 2)
DEL_PLANET = int('10', 2)

OKAY       = int('00', 2)
ENOSTAR    = int('01', 2)
ENOPLANET  = int('11', 2)
ECRANKY    = int('10', 2)

STAR_WIDTH  = 220 
STAR_HEIGHT = 220

def p(s):
    return lambda : sys.stdout.write(s+'\n')

class Harmony(object):
    BG_COLOR = 0,0,0
    R_SIZE = 1024, 768
    screen   = None
    creator  = None
    add_buf  = {"stars": [], "planets": []}
    stars    = {}
    planets  = {}
    starNum  = 0
    planetNum = 0
    angle = 10 
    mscale   = "pentatonic"
   # ctx      = zmq.Context()
   # sock     = ctx.socket(zmq.REQ)
   # sub      = ctx.socket(zmq.SUB)

    def __init__(self, res=(0,0)):
        pygame.init()
        self.screen = pygame.display.set_mode(res,
                                               RESIZABLE|FULLSCREEN)
        self.creator = Creator()

      #  self.sock.connect("tcp://127.0.0.1:5000")
      #  self.sub.connect( "tcp://127.0.0.1:5001")
      # self.sub.setsockopt(zmq.SUBSCRIBE, "")


    def handle_events(self):
         mousePos = self.creator.get_mouse_pos()
	 for event in pygame.event.get():
            if   event.type == pygame.QUIT: sys.exit(0)
            elif event.type == pygame.MOUSEBUTTONDOWN:
               # self.add_buf["stars"].append(Star(mousePos, self.mscale,STAR_HEIGHT, STAR_WIDTH))
                self.stars[self.starNum] = Star(mousePos, self.mscale, STAR_HEIGHT, STAR_WIDTH, self.starNum)
                self.starNum += 1
            elif event.type == pygame.KEYDOWN:
                if event.key == K_ESCAPE:
                    self.screen = pygame.display.set_mode(self.R_SIZE, 0, 0) 
                elif event.key == K_q:
                    sys.exit()
                elif event.key == K_RIGHT:
                    self.creator.scroll_right()
                elif event.key == K_LEFT:
                    self.creator.scroll_left()
                elif event.key == K_DOWN:
                    self.creator.scroll_down()
                elif event.key == K_UP:
                    self.creator.scroll_up()
                elif event.key == K_a:
                    planetAttrs = self.creator.create_planet()
                    planet =  Planet(planetAttrs, self.planetNum, self.angle)
 
                    for sval in self.stars.itervalues():
                        if sval.sid == planet.sid:
                            sval.planets.append(planet)
 
                    self.planetNum += 1
                    self.angle += 10

    def graphics_tick(self):
        self.handle_events()
        self.creator.update_universe(self.stars, len(self.stars))
        self.creator.draw_universe(self.screen) 
 
        pygame.display.flip()

   # def network_tick(self):
   #     wrld = self.sub.recv(zmq.NOBLOCK)
   #     print wrld

    #    self.sock.send(pack('!B', ADD_STAR))
     #   result = unpack('!B', self.sock.recv())
     #   { OKAY:      p("Okay"),
     #     ENOSTAR:   p("No such star"),
     #     ENOPLANET: p("No such planet"),
     #     ECRANKY:   p("Server was just cranky")
     #    }.get(result[0], p("Woops"))()

    def snd_tick(self):
        pass

    def run(self):
        clock = pygame.time.Clock()
        while True:
            self.screen.fill(self.BG_COLOR)
            #self.network_tick()
            self.graphics_tick()
            self.snd_tick()
            clock.tick(40)

if __name__ == "__main__":
    Harmony().run()
