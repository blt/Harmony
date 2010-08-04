import pygame, os
import math
import operator
from pygame import image
from lookupCoord import * 

class Planet(pygame.sprite.Sprite):
    image_pth = os.path.join(os.environ["HARMONY_MEDIA_DIR"],
                             "planet.png")
   # image_pth = os.path.join('../media', 'planet.png')
    image = None
    rect  = None
    lookUp = None
    startpos = 0
    pos = 0
    pid = 0
    speed = 0
    radius = 0
    angle = 0
    vertx = 0
    verty = 0
    time = 0

    # attrs = (radius, speed, angle)
    def __init__(self, attrs, pid, time):
        pygame.sprite.Sprite.__init__(self)
        self.image = image.load(self.image_pth).convert();
        self.time = time
        self.radius = attrs[0]
        self.speed = attrs[1]
        self.pid = pid
        self.angle = attrs[2]
        self.rect = self.image.get_rect()
        self.lookUp = lookupCoord(0, 0, self.radius, self.angle)
        self.set_startpos()
        self.update()

    def get_image(self):
        return self.image

    def set_startpos(self):
        fp = self.lookUp.firstPosition
        self.pos = (fp + (self.time * self.speed)) % self.lookUp.CIndex
        loc = self.lookUp.getLocation(int(self.pos))
        self.vertx = loc[1]
        self.verty = loc[2]
                
    def update(self):
        self.rect.center = (self.vertx, self.verty)
        
        self.pos += self.speed
        loc = self.lookUp.getLocation(int(self.pos))
        self.vertx = loc[1]
        self.verty = loc[2]
   
