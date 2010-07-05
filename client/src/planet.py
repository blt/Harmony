import pygame
import math
import operator
from pygame import image

class Planet(object):
	
	def __init__(self, radius, startpos):
	    self.image = image.load("planet.png").convert();
	    self.planet = self.image.get_rect(center = startpos)
            self.radius = radius 
            self.vertx = 500
	    self.verty = 350
            self.x = 0
	    self.y = 0

	def get_image(self):
	    return self.image

   	def get_planet_rect(self):
	    return self.planet

	def rotate(self, angle_degrees):
   	    radians = math.radians(angle_degrees)
	    cos = math.cos(radians)
	    sin = math.sin(radians)
            self.x = self.vertx + (cos*self.radius)
            self.y = self.verty + (sin*self.radius)
                        

	def update_pos(self):
	    self.planet.center = (self.x, self.y)
   
