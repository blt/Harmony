from pygame import image
import os, os.path, pygame


class Ring(object):
    
    image_pth = None
    image = None
    rect = None
    
    def __init__(self, pos, name):
        self.image_pth = os.path.join('media', name)
        self.image = image.load(self.image_pth).convert()
        self.rect = self.image.get_rect(center = pos)
        self.image.set_colorkey(self.image.get_at((0,0)), pygame.RLEACCEL)
  
    
