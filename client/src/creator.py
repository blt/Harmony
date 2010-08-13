import pygame, math
import time
from star import Star
from solarsystem import SolarSystem
from lookupCoord import *
from planet import Planet
from pygame import Surface
from pygame import mouse
from pygame import Rect

NOTES = ['A','A#','B','C','C#','D',
         'D#','E','F','F#','G','G#']

# Contains indexes into the notes list
# for each key based on the pentatonic 
# scale
PENTATONIC = dict({ 'F': [8,3,10,5,0],
                    'C': [3,10,5,0,7],
                    'G': [10,5,0,7,2],
                    'D': [5,0,7,2,9],
                    'A': [0,7,2,9,4],
                    'E': [7,2,9,4,11],
                    'B': [2,9,4,11,6],
                   'F#': [9,4,11,6,1],
                   'C#': [4,11,6,1,8],
                   'G#': [11,6,1,8,3],
                   'D#': [6,1,8,3,10],
                   'A#': [1,8,3,10,5]})
                

class Creator(object):

    CoU = [] #center of the universe
    view = None
    viewrect = None

    # surface used to clear view sprites
    viewClear = None 

    # pSprites is a group that holds all 
    # planets in the universe.  It is used
    # only for tracking planet collisions.
    pSprites = None 

    # dictionary of star ids as keys and 
    # a list of planets as values.  Any 
    # planet involved in a collision will
    # be added to this dictionary
    collSet = set([]) 
    
    # ssSprites contains all the solarsystems
    # and will allow the creator to update 
    # and draw them.
    ssSprites = None

    # default music scale is pentatonic for
    # harmonic quality.  More scales can be
    # added at a future date.
    mscale = None
    mousePos = (0, 0)
    xbound = 0
    ybound = 0
    scrollDist = 10
    uSize = 0
    screen = None
    timestamp = (0,0,0)

    # time in milliseconds since last update
    timepassed = 0
    prevTime = 0
    
    # rotation length is the angle in degrees
    # a planet moves in one tick
    rot_length = 2
    
    def __init__(self, screen, mscale=PENTATONIC):
        self.mscale = mscale
        self.ssSprites = pygame.sprite.RenderUpdates()
        self.pSprites = pygame.sprite.RenderUpdates()
        self.view = Surface(screen.get_size())
        self.viewrect = self.view.get_rect()
        self.viewClear = Surface(screen.get_size())
        self.screen = screen
        self.CoU = [screen.get_width()/2, screen.get_height()/2]
	self.timestamp = time.time()

    # Whenever there is an update from the network
    # a new timestamp is sent which is the time
    # elapsed since the universe began. 
    def update_time(self, time):
	self.prevTime = self.timestamp
	self.timestamp = time  
 
    # Update universe takes a list of update tuples
    # as such [('system', starid, xpos, ypos, key, 
    #          [('planet', planetid, angle, speed, radius), ...])
    #          , ... ]
    # If a system exists check for new planets and add
    # otherwise create a new system and add to sprite 
    # group 
    def update_universe(self, update, del_flag):
        #indexes in the update list   
        id = 1
	time = 5
	sysList = self.ssSprites.sprites()
	pExists = []
        pDict = {} 
	
        # print update 
        # iterate through list add any objects that 
        # dont already exist 
	for system in update:
            ss = self.system_exists(system[id])
	    pExists = []
	    if ss:
                planet_list = system[6]
                for planet in planet_list:
                     p = self.planet_exists(planet[id], ss)
		     pExists.append(planet[id])
                     if not p:
			 timepassed = self.prevTime - planet[5]
                         newPlanet = ss.add_planet(planet, timepassed)
                         # Whenever a new planet is created must be
                         # added to collision sprite group
                         self.pSprites.add(newPlanet)
	        pDict[ss.star.sid] = pExists     	
                   
            else:
                self.add_system(system)

        if (del_flag == 1):
	   self.remove_planets(pDict)

           # check for deletes
	   updated_ids = [upd[id] for upd in update]
	   for sys in sysList:
               if not sys.star.sid in updated_ids:
		   for p in sys.pSprites.sprites():
		       p.sound.stop()
		   self.ssSprites.remove(sys)
        

    def remove_planets(self, pDict):
	
	ssList = self.ssSprites.sprites()

	for sys in ssList:
	    pList = sys.pSprites.sprites()
	  #  print sys.star.sid
	  #  for key, val in pDict[sys.star.sid]:
          #      for planet in pList:
          #          if not planet.pid in val:
	  #              sys.pSprites.remove(planet)
          #               self.pSprites.remove(planet)      
   
    # Create a new solar system 
    def add_system(self, system):
        sid = 1
        xpos = 2
        ypos = 3
        key = 4
        
        print "creating system", system
        print "xpos:", system[xpos]
        print "ypos:", system[ypos]
        print "id:", system[sid]
        print "key:", system[key]
        
        pos = (system[xpos], system[ypos])                
        sys = SolarSystem(pos, system[sid], system[key])
        planet_list = system[6]
        
        for planet in planet_list:
            newPlanet = sys.add_planet(planet, self.timepassed)
            # Whenever a new planet is created must be
            # added to collision sprite group
            self.pSprites.add(newPlanet)

        self.ssSprites.add(sys)      
                   
    # check if a planet already exists in a solar
    # system.
    def planet_exists(self, pid, system):
       for planet in system.pSprites.sprites():
           if planet.pid == pid:
                return planet
       
       return None
 

    # iterate through ssSprite group and compare ids
    # with id of update star.  If id found return the 
    # star object, otherwise return None
    def system_exists(self, sys_id):
        for system in self.ssSprites.sprites():
            if sys_id == system.star.sid:
               # print "System exists", sys_id		  
                return system
  
        return None

    # return a planet's attrs (radius, pos, starid)
    # determined by mouse position relative
    # to vertex of home star 
    def create_planet(self):
        mpos = self.get_mouse_pos()
        homeStar = 0
        mdistx  = 0
        mdisty  = 0
        radius = 0
        ringNum = 0
        angle = 0
        note = None        
        found = 0
     
        # Iterate through solar systems and 
        # determine which one planet is added
        # to.  Set mdist, mdisty: positions
        # relative to the center of system.
        sslist = self.ssSprites.sprites()
        for ss in sslist:
            ss.update_pos(self.xbound, self.ybound)
            if ss.rect.collidepoint(mpos[0], mpos[1]):
                found = 1
                if mpos[0] > ss.relposx:
                    mdistx = mpos[0] - ss.relposx
                else:
                    mdistx = ss.relposx - mpos[0]
 
                if mpos[1] > ss.relposy:
                    mdisty = mpos[1] - ss.relposy
                else:
                    mdisty = ss.relposy - mpos[1]
                
		# hypotenuese is the relative distance 
		# from vertex
		hypot = math.hypot(mdistx, mdisty)              
		
		# get actual radius based on range of
		# magnitude of hypot 
		if hypot < 40:
		    radius = 30
                    ringNum = 0
		elif hypot >= 40 and hypot < 60:
		    radius = 50
 		    ringNum = 1
		elif hypot >= 60 and hypot < 80:
		    radius = 70
		    ringNum = 2
		elif hypot >= 80 and hypot < 100:
		    radius = 90
	 	    ringNum = 3
		else:
		    radius = 110
		    ringNum = 4
	  
		angleRad = math.atan2(mdisty, mdistx)
		angle = int(math.degrees(angleRad)) 
		
        	# Get note based on key of system
                # and the ring it on 
                note = self.get_note(ss, ringNum)
                homestar = ss.star.sid
		if found:
            	    return (homestar, angle, radius, note) 
        
        return 0    


    # Returns the note of the newly created planet 
    # based on key of solar system  and radius
    def get_note(self, ss, ringNum):
        return self.mscale[ss.key][ringNum]
        
    # Return mouse position relative to the view 
    def get_mouse_pos(self):
        pos = mouse.get_pos()
        xpos = pos[0] + self.xbound
        ypos = pos[1] + self.ybound

        return (xpos, ypos)   
 
    def scroll_right(self):
        self.xbound += self.scrollDist
        self.CoU[0] += self.scrollDist

    def scroll_left(self):
        if self.xbound >= self.scrollDist:
            self.xbound -= self.scrollDist
            self.CoU[0] -= self.scrollDist

    def scroll_down(self):
        self.ybound += self.scrollDist
        self.CoU[1] += self.scrollDist

    def scroll_up(self):
        if self.ybound >= self.scrollDist:
            self.ybound -= self.scrollDist
            self.CoU[1] -= self.scrollDist

    def fast_scroll_right(self):
	self.xbound += 1000
        self.CoU[0] += 1000

    def fast_scroll_left(self):
        if self.xbound >= 1000:
            self.xbound -= 1000
            self.CoU -= 1000

    def fast_scroll_down(self):
        self.ybound += 1000
        self.CoU += 1000

    def fast_scroll_up(self):
        if self.ybound >= 1000:
            self.ybound -= 1000
            self.CoU -= 1000

    def warp_speed_right(self):
        self.xbound += 10000
        self.CoU += 10000
 
    def warp_speed_left(self):
	if self.xbound >= 10000:
	    self.xbound -= 10000
            self.CoU -= 10000

    def warp_speed_down(self):
	self.ybound += 10000
        self.CoU += 10000

    def warp_speed_up(self):
	if self.ybound >= 10000:
            self.ybound -= 10000

    def update_collisions(self):
        collplanets = []
        self.collSet = set([]) # empty the set
 	for planet in self.pSprites.sprites():
            self.pSprites.remove(planet)
            collplanets += pygame.sprite.spritecollide(planet, self.pSprites, False)
            self.pSprites.add(planet)
 
        #eliminate duplicates
        self.collSet = set(collplanets)
    
    # checks to see if a star of planet
    # has been selected. Returns a tuple
    # containing selected items.  if planet
    # is selected a start will always be
    # as well.
    def check_selected(self):
	mpos = self.get_mouse_pos()
	star = 0
        planet = 0
	
	for ss in self.ssSprites.sprites():
	    if ss.rect.collidepoint(mpos):
		star = ss.star
                print ss.rect.center 
                print mpos
		pos = self.get_relpos(mpos, ss.rect.center) 
		for p in self.pSprites.sprites():
	    	    if p.rect.collidepoint(pos):
			planet = p
 	
	return (star, planet) 
	
    def get_relpos(self, mpos, pos):
	relposx = 0
        relposy = 0
	if mpos[0] > pos[0]:
	    relposx =  mpos[0] - pos[0]
	else:
	    relposx = pos[0] - mpos[0]

	if mpos[1] > pos[1]:
	    relposy = mpos[1] - pos[1]
        else:
	    relposy = pos[1] - mpos[1]

        return (relposx + 120, relposy + 120) 
        
    def draw_universe(self):
        self.ssSprites.clear(self.view, self.viewClear)
        for ss in self.ssSprites.sprites():
	    ss.update_pos(self.xbound, self.ybound)
            ss.update_focus(mouse.get_pos())

        self.ssSprites.update(self.CoU, self.collSet)
        self.ssSprites.draw(self.view)

        self.screen.blit(self.view, self.viewrect)
