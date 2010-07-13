from pygame import image
import os, os.path, pygame
from ring import Ring
from planet import Planet

class Star(object):
   # image_pth = os.path.join(os.environ["HARMONY_MEDIA_DIR"],
   #                          "star.png")
   # image_r1pth = os.path.join(os.environ["HARMONY_MEDIA_DIR"],
   #                          "ring1.png")

    image_pth = os.path.join('../media', 'sun.xcf')
    image = None
    rect = None
    ring1 = None
    vertex = 0, 0
    rings = []
    planets = []
    notes = 0
    size = 0

    def __init__(self, pos, scale):
        self.image = image.load(self.image_pth).convert()
        self.rect = self.image.get_rect(center = pos)
        self.image.set_colorkey(self.image.get_at((0,0)), pygame.RLEACCEL)
        self.vertex = pos
        self.size = self.rect.size
            
        if(scale == 'pentatonic'):
            self.notes = 5
            
        self.create_rings()
        
    def create_rings(self):
        for i in xrange(self.notes):
            rname = ['ring', `i`, '.xcf']
            self.rings.append(Ring(self.vertex, ''.join(rname)))
            
    def zoom(self):
        #self.size = self.size*2
        self.image = pygame.transform.scale2x(self.image)
        #self.rect.inflate(self.size)
        
        
        
