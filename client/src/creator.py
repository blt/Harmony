import pygame, math
from star import Star
from pygame import Surface
from pygame import mouse
from pygame import Rect


class Creator(object):

    CoU = None #center of the universe
    scaleFactor = 0
    scaleAmt = 100
    starNum = 0
    starDict = {}
    starSize = (30, 30)
    universe = None
    universeRect = None
    view = None
    viewWidth = 0
    viewHeight = 0
    viewrect = None
    mscale = None
    starSprites = None
    ringSprites = None
    planetSprites = None
    mousePos = (0, 0)
    xbound = 0
    ybound = 0
    scrollDist = 10
    uSize = 0
    

    def __init__(self, mscale='pentatonic'):
        self.uSize = pygame.display.get_surface().get_size()
        self.universe = pygame.Surface(self.uSize)
        self.universeRect = self.universe.get_rect()
        self.CoU = self.universeRect.center
        self.mscale = mscale
        self.mousePos = mouse.get_pos()
        self.starSprites = pygame.sprite.RenderUpdates()
        self.ringSprites = pygame.sprite.RenderUpdates()
        self.planetSprites = pygame.sprite.RenderUpdates()
    
    # called every clock tick: 
    # empty sprite groups
    # fill based on dict passed in from 
    # network.  Rings will only appear if 
    # mouse positioned over star    
    def update_universe(self, uDict, numStars):
        self.starDict = uDict
        self.starSprites.empty()
        self.ringSprites.empty()
	self.planetSprites.empty()

	pos = self.get_mouse_pos()

        self.viewWidth = self.uSize[0] 
        self.viewHeight = self.uSize[1] 
        self.view = Surface((self.viewWidth, self.viewHeight))
        self.viewrect = Rect((0,0), (self.viewWidth, self.viewHeight))
        self.viewrect.center = self.CoU

        for sval in self.starDict.itervalues():
              if self.viewrect.contains(sval.rect):
                  sval.set_pos(self.xbound, self.ybound)
                  self.starSprites.add(sval)
                  
                  for planet in sval.planets:
                      self.planetSprites.add(planet)
              
	      if sval.ssrect.collidepoint(pos[0], pos[1]):
                  for ring in sval.rings:
                      self.ringSprites.add(ring)
        
        self.starNum = numStars

    # return a planet's attrs (radius, pos, starid)
    # determined by mouse position relative
    # to vertex of home star 
    def create_planet(self):
        mpos = self.get_mouse_pos()
        homeStar = 0
        ringNum  = 0
        mdistx  = 0
        mdisty  = 0
        radius = 0
        pos = (0, 0)
        
        for sval in self.starDict.itervalues():
            if sval.ssrect.collidepoint(mpos[0], mpos[1]):
               homeStar = sval.sid
 
               if mpos[0] > sval.relposx:
                   mdistx = mpos[0] - sval.relposx
               else:
                   mdistx = sval.relposx - mpos[0]
 
               if mpos[1] > sval.relposy:
                   mdisty = mpos[1] - sval.relposy
               else:
                   mdisty = sval.relposy - mpos[1]

        mrad = math.hypot(mdistx, mdisty)              
         
        if mrad < 80:
            radius = 60
        elif mrad >= 80 and mrad < 120:
            radius = 100
        elif mrad >= 120 and mrad < 160:
            radius = 140
        elif mrad >= 160 and mrad < 200:
            radius = 180
        else:
            radius = 220
         
        pos = (sval.relposx, (sval.relposy + radius))

        return (radius, pos, homeStar)     
 
    def get_mouse_pos(self):
        pos = mouse.get_pos()
        xpos = pos[0] + self.xbound
        ypos = pos[1] + self.ybound

        return (xpos, ypos)   
 
    def scroll_right(self):
        self.xbound += self.scrollDist

    def scroll_left(self):
        if self.xbound >= self.scrollDist:
            self.xbound -= self.scrollDist

    def scroll_down(self):
        self.ybound += self.scrollDist

    def scroll_up(self):
        if self.ybound >= self.scrollDist:
            self.ybound -= self.scrollDist

    def draw_universe(self, screen):
        self.ringSprites.clear(self.view, Surface(screen.get_size()))
        self.ringSprites.draw(self.view)
        self.starSprites.clear(self.view, Surface(screen.get_size()))
        self.starSprites.draw(self.view)
        self.planetSprites.clear(self.view, Surface(screen.get_size()))
        self.planetSprites.draw(self.view)

        self.viewrect = Rect((0,0), screen.get_size())
        screen.blit(self.view, self.viewrect)
