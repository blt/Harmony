from pygame import image, Surface
import os, os.path, pygame
from ring import Ring
from planet import Planet

class Star(pygame.sprite.Sprite):
    image_pth = os.path.join(os.environ["HARMONY_MEDIA_DIR"],
                             "sun.xcf")
    image      = None
    rect       = None
    origImage  = None
    vertx      = 0
    verty      = 0
    rings      = []
    planets    = {} 
    notes      = 0
    absWidth   = 0
    absHeight  = 0
    relHeight  = 0
    relWidth   = 0
    relposx    = 0
    relposy    = 0
    scaleAmnt  = 10
    xdist      = 10 
    ydist      = 10
    numPlanets = 0
   

    def __init__(self, pos, scale, width, height):
        pygame.sprite.Sprite.__init__(self)
        self.vertx = pos[0]
        self.verty = pos[1]
        self.relposx = self.vertx
        self.relposy = self.verty
        self.absWidth = width
	self.absHeight = height

        if(scale == 'pentatonic'):
            self.notes = 5
        
	self.load_image()    
#       self.create_rings()

    def load_image(self):
        self.image = image.load(self.image_pth).convert()
        self.rect = self.image.get_rect(center = (self.relposx, self.relposy))
        self.image.set_colorkey(self.image.get_at((0,0)), pygame.RLEACCEL)
        self.origImage = self.image        

    def create_rings(self):
        for i in xrange(self.notes):
            rname = ['ring', `i`, '.xcf']
            self.rings.append(Ring(self.vertex, ''.join(rname)))

    def set_pos(self, xbound, ybound):
        self.relposx = self.vertx - xbound
        self.relposy = self.verty - ybound
        self.rect = self.image.get_rect(center = (self.relposx, self.relposy))
            
    def zoom(self, CoU, scaleFactor):
        if(self.vertx > CoU[0]):
            self.relposx = self.relposx + (scaleFactor * self.xdist)
        elif(self.vertx < CoU[0]):
            self.relposx = self.relposx - (scaleFactor * self.xdist)
    
        if(self.verty > CoU[1]):
            self.relposy = self.relposy + (scaleFactor * self.ydist)
        elif(self.verty < CoU[1]):
            self.relposy = self.relposy - (scaleFactor * self.ydist)

        self.scale_image(scaleFactor, 1)

    def unzoom(self, CoU, scaleFactor):
        if(self.vertx > CoU[0]):
            self.relposx = self.relposx - (scaleFactor * self.xdist)
        elif(self.vertx < CoU[0]):
            self.relposx = self.relposx + (scaleFactor * self.xdist)
    
        if(self.verty > CoU[1]):
            self.relposy = self.relposy - (scaleFactor * self.ydist)
        elif(self.verty < CoU[1]):
            self.relposy = self.relposy + (scaleFactor * self.ydist)

        self.scale_image(scaleFactor, 0)
 
    def scale_image(self, scaleFactor, ztype):
        if ztype == 1:
            print scaleFactor, self.scaleAmnt, self.absWidth            
            self.relWidth = scaleFactor * self.scaleAmnt + self.absWidth  
            self.relHeight = scaleFactor * self.scaleAmnt + self.absHeight

	else:
            self.relWidth = scaleFactor * self.scaleAmnt - self.absWidth  
            self.relHeight = scaleFactor * self.scaleAmnt - self.absHeight
 
        print self.relWidth, self.relHeight
 	
        self.image = pygame.transform.smoothscale(self.origImage, (self.relWidth, self.relHeight))
        self.rect = self.image.get_rect(center = (self.relposx, self.relposy))
    
