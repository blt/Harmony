from pygame import image, Surface
import os, os.path, pygame
from ring import Ring
from planet import Planet

class Star(pygame.sprite.Sprite):
    image_pth = os.path.join(os.environ["HARMONY_MEDIA_DIR"],
                             "sun.xcf")
    image      = None
    rect       = None
    height     = 0
    width      = 0
    vertx      = 0
    verty      = 0
    rings      = []
    planets    = [] 
    notes      = 0
    relposx    = 0
    relposy    = 0
    numPlanets = 0
    sid        = 0
   

    def __init__(self, pos, scale, height, width, id):
        pygame.sprite.Sprite.__init__(self)
        self.vertx = pos[0]
        self.verty = pos[1]
        self.relposx = self.vertx
        self.relposy = self.verty
        self.sid = id
        self.height = height
        self.width = width

        if(scale == 'pentatonic'):
            self.notes = 5
        
	self.load_image()    
        self.create_rings()

    def load_image(self):
        self.image = image.load(self.image_pth).convert()
        self.rect = self.image.get_rect(center = (self.relposx, self.relposy))
        self.ssrect =  pygame.Rect((0,0), (self.height, self.width))
        self.ssrect.center = (self.relposx, self.relposy)
        self.image.set_colorkey(self.image.get_at((0,0)), pygame.RLEACCEL)

    def create_rings(self):
        for i in xrange(self.notes):
            rname = ['ring', `i`, '.xcf']
            self.rings.append(Ring((self.vertx, self.verty), i, ''.join(rname)))

    def add_planets(self, radius):
        startpos = (self.vertx, self.verty - radius)
        self.planets.append(Planet(radius, startpos))


    def set_pos(self, xbound, ybound):
        self.relposx = self.vertx - xbound
        self.relposy = self.verty - ybound
        self.rect = self.image.get_rect(center = (self.relposx, self.relposy))
            
    
