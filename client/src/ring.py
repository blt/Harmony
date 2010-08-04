from pygame import image
import os, os.path, pygame
from planet import Planet


class Ring(pygame.sprite.Sprite):
    
    image = None
    rect = None
    number = 0
    vertx = 0
    verty = 0
    
    def __init__(self, pos, num, name):
        pygame.sprite.Sprite.__init__(self)
        self.image_pth = os.path.join(os.environ["HARMONY_MEDIA_DIR"],
       					name)
       # self.image_pth = os.path.join('../media', name)
        self.image = image.load(self.image_pth).convert()
        self.rect = self.image.get_rect(center = pos)
        self.image.set_colorkey(self.image.get_at((0,0)), pygame.RLEACCEL)
        self.vertx = pos[0]
        self.verty = pos[1]
        self.number = num

    
         
        
    
