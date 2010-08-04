import sys, pygame
from pygame import FULLSCREEN, DOUBLEBUF, RESIZABLE
from pygame import mouse
from pygame.locals import *
from star import Star
from creator import Creator
from planet import Planet
from updateQ import UpdateQ
from harmonyConn import harmonyConn #Jared's module
from harmonyUi import HarmonyUi

BG_COLOR = 0,0,0
RESIZE = 1024, 768

def p(s):
    return lambda : sys.stdout.write(s+'\n')

class Harmony(object):
    screen = None
    creator = None
    updateQ = None
    hConn = None
    ui = None
    update = None
    
    def __init__(self, res=(0,0)):
        pygame.init()
        self.screen = pygame.display.set_mode(res,
                                               RESIZABLE|FULLSCREEN)
        self.creator = Creator(self.screen)
        self.hConn = harmonyConn('sundhome.com', 1234)
        self.updateQ = UpdateQ(self.hConn)
        self.ui = HarmonyUi(self.screen, self.creator.get_mouse_pos())

    def parse_update(self, update):
        if update[0] == 'universe':
            self.creator.update_time(self.updateQ.timestamp)
            self.creator.update_universe(update[2])
            

    def handle_events(self):
         mousePos = self.creator.get_mouse_pos()
	 for event in pygame.event.get():
            if   event.type == pygame.QUIT: sys.exit(0)
            elif event.type == pygame.KEYDOWN:
                if event.key == K_ESCAPE:
                    self.screen = pygame.display.set_mode(RESIZE, 0, 0) 
                elif event.key == K_q:
                    sys.exit()
                elif event.key == K_s:
                    self.hConn.addStar(mousePos[0], mousePos[1], 
               			       self.ui.get_edit_key())
                elif event.key == K_RIGHT:
                    self.creator.scroll_right()
                elif event.key == K_LEFT:
                    self.creator.scroll_left()
                elif event.key == K_DOWN:
                    self.creator.scroll_down()
                elif event.key == K_UP:
                    self.creator.scroll_up()

    def graphics_tick(self):
        self.handle_events()
        self.creator.draw_universe()
        pygame.display.flip()

    def network_tick(self):
        if self.updateQ.check_for_update():
            update = self.updateQ.get_next_update()
	  #  print update
            if update:
                self.parse_update(update)     
            
#    def snd_tick(self):

    def run(self):
        clock = pygame.time.Clock()

        while True:
            self.screen.fill(BG_COLOR)
            self.network_tick()
            self.graphics_tick()
          #  self.snd_tick()
            clock.tick(40)

if __name__ == "__main__":
    Harmony().run()
