import sys, pygame, zmq
from pygame import FULLSCREEN, DOUBLEBUF, RESIZABLE
from pygame import mouse
from pygame.locals import *
from star import Star
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

def p(s):
    return lambda : sys.stdout.write(s+'\n')

class Harmony(object):
    BG_COLOR = 0,0,0
    R_SIZE = 1024, 768
    screen   = None
    planets  = []
    stars    = [] 
    add_buf  = {"stars": [], "planets": []}
    ctx      = zmq.Context()
    sock     = ctx.socket(zmq.REQ)
    sub      = ctx.socket(zmq.SUB)

    def __init__(self, res=(0,0)):
        pygame.init()
        self.screen = pygame.display.set_mode(res,
                                               RESIZABLE|FULLSCREEN)
        self.sock.connect("tcp://127.0.0.1:5000")
        self.sub.connect( "tcp://127.0.0.1:5001")
        self.sub.setsockopt(zmq.SUBSCRIBE, "")

    def graphics_tick(self):
        for event in pygame.event.get():
            if   event.type == pygame.QUIT: sys.exit()
            elif event.type == pygame.MOUSEBUTTONDOWN:
                #self.add_buf["stars"].append(event.pos))
                self.stars.append(Star(event.pos))
            elif event.type == pygame.KEYDOWN:
                if event.key == K_ESCAPE:
                    self.screen = pygame.display.set_mode(self.R_SIZE, 0, 0) 
                elif event.key == K_q:
                    sys.exit()
                                                          
                
        for star in self.stars:
            self.screen.blit(star.image, star.rect)
            if(star.rect.collidepoint(mouse.get_pos())):
                for ring in star.rings:
                    self.screen.blit(ring, ring.get_rect(center = star.vertex))

        pygame.display.flip()

    def network_tick(self):
        wrld = self.sub.recv(zmq.NOBLOCK)
        print wrld

        self.sock.send(pack('!B', ADD_STAR))
        result = unpack('!B', self.sock.recv())
        { OKAY:      p("Okay"),
          ENOSTAR:   p("No such star"),
          ENOPLANET: p("No such planet"),
          ECRANKY:   p("Server was just cranky")
         }.get(result[0], p("Woops"))()

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
