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
    del_flag = 0
    
    def __init__(self, res=(0,0)):
        pygame.init()
        pygame.mixer.init()
        self.screen = pygame.display.set_mode(res,
                                               RESIZABLE|FULLSCREEN)
        self.creator = Creator(self.screen)
        self.hConn = harmonyConn('sundhome.com', 1234)
        self.updateQ = UpdateQ(self.hConn)
        self.ui = HarmonyUi(self.screen, self.creator.get_mouse_pos())
      
    def parse_update(self, update):
        if update[0] == 'universe':
            self.creator.update_time(self.updateQ.timestamp)
            self.creator.update_universe(update[2], self.del_flag)
            

    def handle_events(self):
	 mousePos = self.creator.get_mouse_pos()
         self.ui.set_current_pos(mousePos)
         key_pressed = pygame.key.get_pressed()
	 for event in pygame.event.get():
            if   event.type == pygame.QUIT: sys.exit(0)
            elif event.type == pygame.MOUSEBUTTONDOWN:
		sel = self.creator.check_selected()
		if sel[0]:
		    self.ui.set_selected_star(sel[0])
		    if sel[1]:
			self.ui.set_selected_planet(sel[1])
            elif event.type == pygame.KEYDOWN:
                if event.key == K_ESCAPE:
                    self.screen = pygame.display.set_mode(RESIZE, 0, 0) 
                elif event.key == K_q:
                    sys.exit()
	        elif event.key == K_e:
		    self.ui.set_editMode()
                elif event.key == K_s:
                    self.hConn.addStar(mousePos[0], mousePos[1], 
               			       self.ui.get_edit_key())
		elif event.key == K_p:
		    pattr = self.creator.create_planet()
		    starid = pattr[0]
		    angle = pattr[1]
		    print "Angle: ", angle
	            radius = pattr[2]
		    print "Radius: ", radius
		    note = pattr[3]
		    print "Note: ", note

		    speed = self.ui.editSpeed
		    self.hConn.addPlanet(starid, angle, speed,
					 radius, note)
	        elif event.key == K_j:
		    self.ui.inc_edit_speed()
		elif event.key == K_k:
		    self.ui.inc_edit_key()
	        elif event.key ==  K_z:
                    if self.ui.selStarId:
		        self.hConn.delStar(self.ui.selStarId)
                elif event.key == K_x:
	            if self.ui.selStarId and self.ui.PlanetId:
		        self.hConn.delPlanet(self.ui.selStarId, self.ui.selPlanetId) 
         if key_pressed[K_RIGHT]:
             if key_pressed[K_LSHIFT]:
		 self.creator.fast_scroll_right()
             elif key_pressed[K_RSHIFT]:
 		 self.creator.warp_speed_right()
             else:
                 self.creator.scroll_right()
         if key_pressed[K_LEFT]:
             if key_pressed[K_LSHIFT]:
		 self.creator.fast_scroll_left()
             elif key_pressed[K_RSHIFT]:
 		 self.creator.warp_speed_left()
             else:
                 self.creator.scroll_left()
         if key_pressed[K_DOWN]:
             if key_pressed[K_LSHIFT]:
		 self.creator.fast_scroll_down()
             elif key_pressed[K_RSHIFT]:
 		 self.creator.warp_speed_down()
             else:
                 self.creator.scroll_down()
         if key_pressed[K_UP]:
             if key_pressed[K_LSHIFT]:
		 self.creator.fast_scroll_up()
             elif key_pressed[K_RSHIFT]:
 		 self.creator.warp_speed_up()
             else:
                 self.creator.scroll_up()

    def graphics_tick(self):
        self.handle_events()
        self.creator.update_collisions()
        self.creator.draw_universe()
        self.ui.create_fields()
        self.ui.draw_ui()
        self.screen.blit(self.ui.image, self.ui.rect)
        pygame.display.flip()

    def network_tick(self):
        if self.updateQ.check_for_update():
	    self.del_flag = self.updateQ.del_flag
	    print self.del_flag
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
