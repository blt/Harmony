import pygame
from pygame import Surface, Rect
from planet import Planet
from star import Star
from ring import Ring
from lookupCoord import *

## SolarSystem is a container for
## Star, Ring and Planet objects,
## as well as a pygame Surface for
## these objects to be blitted.
## Each Solar System is identified
## by its Star ID. It holds and 
## updates separate Sprite Groups
## for each object type.

NOTES = ['A','A#','B','C','C#','D',
         'D#','E','F','F#','G','G#']

class SolarSystem(pygame.sprite.Sprite):

    pSprites = None
    rSprites = None
    star = None
    image = None
    rect = None
    pos = None
    centerx = 0
    centery = 0
    relposx = 0
    relposy = 0
    size = (0,0)
    key = None
    focus = None

    def __init__(self, pos, id, key, size=(220,220)):
        pygame.sprite.Sprite.__init__(self)
        self.image = pygame.Surface(size).convert()
        self.rect = self.image.get_rect(center=pos)
        self.image.set_colorkey(self.image.get_at((0,0)), pygame.RLEACCEL)
        self.pos = pos
        self.relposx = pos[0]
        self.relposy = pos[1]
        self.pSprites = pygame.sprite.RenderUpdates()
        self.rSprites = pygame.sprite.RenderUpdates()
        self.centerx = size[0]/2
        self.centery = size[1]/2
        self.create_rings()
        self.star = Star((self.centerx, self.centery), id)
        self.image.blit(self.star.image, self.star.rect)
        self.size = size
        self.key = key
        self.focus = 0

    # Add a planet object to the planet 
    # sprite group: pSprites
    # return planet obj to be added to 
    # creator's planet sprite group
    def add_planet(self, planet, time):
        pid = 1
        angle = 2
        speed = 3
        radius = 4
        note = 5
        pos = (self.centerx, self.centery)
        
        attrs = (planet[radius],  
                 planet[speed],
                 planet[angle])
 
        newPlanet = Planet(attrs, planet[pid], pos, time, NOTES[note])
   	self.pSprites.add(newPlanet)

        return newPlanet

    # Used to handle scrolling.  Every tick, the position
    # is updated relative to the view rect in Creator
    def update_pos(self, xbound, ybound):
        self.relposx = self.pos[0] - xbound
        self.relposy = self.pos[1] - ybound
        self.rect.center=(self.relposx, self.relposy) 

    # Determines whether the mouse pointer is contained
    # within the solar system rect
    def update_focus(self, mpos):
        if self.rect.collidepoint(mpos):
	    self.focus = 1
        else:
            self.focus = 0

    # Clear dirty sprites, update planet position and
    # draw sprite groups on self surface.  Only show
    # rings if mouse is within solar system rect: ie
    # self.foucs is set to true
    def update(self, cou, collSet):
        clearBg = Surface(self.size)

        self.pSprites.clear(self.image, clearBg)
        self.rSprites.clear(self.image, clearBg)
        if self.focus:
            self.rSprites.draw(self.image)
 
        vol = self.get_volume(cou)

	self.pSprites.update(vol, collSet)
        self.pSprites.draw(self.image)

        self.image.blit(self.star.image, self.star.rect)

    # Determine the volume of the system based on 
    # proximity to the center of the universe
    def get_volume(self, cou):
        xdist = 0
        ydist = 0
        centerx = self.star.rect.centerx
        centery = self.star.rect.centery
        volume = 0.5
        if centerx < cou[0]:
            xdist = cou[0] - centerx
        else:
	    xdist = centerx - cou[0]
 
        if centery < cou[1]:
            ydist = cou[1] - centery
        else:
	    ydist = centery - cou[1]

	if xdist < 200 and ydist < 200:
	    volume = 0.5
        elif xdist < 400 and ydist < 400:
            volume = 0.4
        elif xdist < 600 and ydist < 600:
            volume = 0.3
        elif xdist < 800 and ydist < 800:
            volume = 0.2
        elif xdist < 1000 and ydist < 1000:
            volume = 0.1
        else:
            volume = 0

        return volume
           


    # Initialize the rings and add them to 
    # sprite group rSprites.
    def create_rings(self):
	for i in xrange(5):
 	    rname = ['ring', `i`, '.xcf']
            self.rSprites.add(Ring((self.centerx, self.centery),
              			    i, ''.join (rname))) 

  
