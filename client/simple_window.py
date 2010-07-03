import sys, pygame
from star import *
from planet import *
from pygame import *
import operator

BG_COLOR = 0,0,0
 
def main():

	pygame.init()

	size = width, height = 1024, 768
        angle = 2
	screen = pygame.display.set_mode(size, 0, 0);
        planets = []
        stars = []
        star_pos = []

	while 1:
		for event in pygame.event.get():
	    	    if event.type == pygame.QUIT: sys.exit()
                    if event.type == pygame.MOUSEBUTTONDOWN: 
			stars.append(Star(event.pos))
                        

                screen.fill(BG_COLOR)
                for star in stars:
			screen.blit(star.get_image(), star.get_star_rect())
                                                
                pygame.display.flip()
               		
                time.wait(10)

   		angle += 2

		if (operator.ge(angle, 360)):
		    angle = 0

if __name__ == '__main__' : main()

