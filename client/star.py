import pygame
from pygame import image


class Star(object):

	def __init__(self, (xpos, ypos)):
            self.image = image.load("star.png").convert()
            self.star = self.image.get_rect(center = (xpos, ypos))    

	def get_image(self):
	    return self.image

        def get_star_rect(self):
	    return self.star

