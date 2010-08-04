import pygame, math
from star import Star
from solarsystem import SolarSystem
from lookupCoord import *
from planet import Planet
from pygame import Surface
from pygame import mouse
from pygame import Rect

PENTATONIC = dict({ 'F': ['F','C','G','D','A'],
                    'C': ['C','G','D','A','E'],
                    'G': ['G','D','A','E','B'],
                    'D': ['D','A','E','B','F#'],
                    'A': ['A','E','B','F#','C#'],
                    'E': ['E','B','F#','C#','A#'],
                    'B': ['B', 'F#','C#','G#','D#'],
                   'F#': ['F#','C#','G#','D#','A#'],
                   'C#': ['C#','G#','D#','A#','F'],
                   'G#': ['G#','D#','A#','F','C'],
                   'D#': ['D#','A#','F','C','G'],
                   'A#': ['A#','F','C','G','D']})
                

class Creator(object):

    CoU = None #center of the universe
    view = None
    viewrect = None

    # surface used to clear view sprites
    viewClear = None 

    # pSprites is a group that holds all 
    # planets in the universe.  It is used
    # only for tracking planet collisions.
    pSprites = None 
    
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
    timestamp = (0,0)

    # time in milliseconds since last update
    timepassed = 0
    
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

    # Whenever there is an update from the network
    # a new timestamp is sent which is the time
    # elapsed since the universe began. 
    def update_time(self, time):
        secs = time[0] - self.timestamp[0]
        usecs = time[1] - self.timestamp[1]
        self.timestamp = time        
        self.timepassed = (usecs * math.pow(10, -3)) + (secs * math.pow(10, 3)) 
 
    
    # Update universe takes a list of update tuples
    # as such [('system', starid, xpos, ypos, key, 
    #          [('planet', planetid, angle, speed, radius), ...])
    #          , ... ]
    # If a system exists check for new planets and add
    # otherwise create a new system and add to sprite 
    # group 
    def update_universe(self, update):
        #indexes in the update list   
        id = 1
       # print update 
        # iterate through list add any objects that 
        # dont already exist 
	for system in update:
            ss = self.system_exists(system[id])
	    if ss:
                planet_list = system[5]
                for planet in planet_list:
                     p = self.planet_exists(planet[id], ss)
                     if not p:
                         pos = get_planet_pos(planet)
                         newPlanet = ss.add_planet(planet, pos)
                         # Whenever a new planet is created must be
                         # added to collision sprite group
                         self.pSprites.add(newPlanet)
            else:
                self.add_system(system)
   
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
        planet_list = system[5]
        
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
        found = false
     
        # Iterate through solar systems and 
        # determine which one planet is added
        # to.  Set mdist, mdisty: positions
        # relative to the center of system.
        sslist = self.ssSprites.sprites()
        for ss in sslist:
            if ss.rect.collidepoint(mpos[0], mpos[1]):
                found = true
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
		if hypot < 80:
		    radius = 60
                    ringNum = 0
		elif hypot >= 80 and hypot < 120:
		    radius = 100
 		    ringNum = 1
		elif hypot >= 120 and hypot < 160:
		    radius = 140
		    ringNum = 2
		elif hypot >= 160 and hypot < 200:
		    radius = 180
	 	    ringNum = 3
		else:
		    radius = 220
		    ringNum = 4
	  
		angleRad = math.atan2(mdisty, mdistx)
		angle = math.degrees(angleRad) 
		
        	# Get note based on key of system
                # and the ring it on 
                note = self.get_note(ss, ringNum)
                speed = 0 # default speed slowest

		if found:
            	    return (radius, angle, homeStar, note) 
        
        return 0    


    # Returns the note of the newly created planet 
    # based on key of solar system  and radius
    def get_note(self, ss, ringNum):
        return mscale[ss.key][ringNum]
        
    # Return mouse position relative to the view 
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

    def draw_universe(self):
        self.ssSprites.clear(self.view, self.viewClear)
        for ss in self.ssSprites.sprites():
            print "inc"
	    ss.update_pos(self.xbound, self.ybound)

        self.ssSprites.update()
        self.ssSprites.draw(self.view)

        self.screen.blit(self.view, self.viewrect)
