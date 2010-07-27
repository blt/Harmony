import pygame, os
import math
import operator
from pygame import image

class Planet(pygame.sprite.Sprite):
    image_pth = os.path.join(os.environ["HARMONY_MEDIA_DIR"],
                             "planet.png")
   # image_pth = os.path.join('../media', 'planet.png')
    image = None
    rect  = None
    pid = 0
    sid = 0
    radius = 0
    vertx = 0
    verty = 0
    x = 0
    y = 0

    # attrs = (radius, startpos, sid)
    def __init__(self, attrs, pid, angle):
        pygame.sprite.Sprite.__init__(self)
        self.image = image.load(self.image_pth).convert();
        self.radius = attrs[0]
        self.sid = attrs[2]
        self.pid = pid
        startpos = attrs[1] 
        self.vertx = startpos[0]
        self.rect = self.image.get_rect(center = startpos)
        self.verty = startpos[1]
        self.x = 0
        self.y = 0
        self.rotate(angle)

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
   
