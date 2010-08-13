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
    time = 0
    note = None
    sound = None
    volume = 0.7

    # attrs = (radius, speed, angle)
    def __init__(self, attrs, pid, pos, time, note):
        pygame.sprite.Sprite.__init__(self)
        self.image = image.load(self.image_pth).convert();
        self.note = note
        self.time = time
        self.radius = attrs[0]
        self.speed = attrs[1]
        self.pid = pid
        self.angle = attrs[2]
        self.rect = self.image.get_rect()
        self.lookUp = lookupCoord(pos[0], pos[1], self.radius, self.angle)
        self.set_startpos()
        self.load_sound()
        self.sound.play(-1)
        self.update(0.5, None)

    def load_sound(self):   
        str = [self.note, '_oct1.ogg']
	filename = ''.join(str)
        snd_pth = os.path.join(os.environ['HARMONY_MEDIA_DIR'], 
                                          filename)
        print snd_pth
        self.sound = pygame.mixer.Sound(snd_pth)

    def get_image(self):
        return self.image

    def set_startpos(self):
        fp = self.lookUp.firstPosition
        self.pos = (fp + (self.time * self.speed)) % self.lookUp.CIndex
        loc = self.lookUp.getLocation(int(self.pos))
        self.vertx = loc[1]
        self.verty = loc[2]
                
    def update(self, max_vol, collSet):
        self.rect.center = (self.vertx, self.verty)
        
        self.pos += self.speed
        loc = self.lookUp.getLocation(int(self.pos))
        self.vertx = loc[1]
        self.verty = loc[2]

	if collSet:
            for p in collSet:
                if p.radius == self.radius:
                   # if self.volume > 0:
                   #     self.volume -= 0.005 
                   self.volume = 0 
	else:
           # if self.volume < max_vol:
	   #	self.volume += 0.005
           # else:
	    self.volume = max_vol

        self.sound.set_volume(self.volume)
