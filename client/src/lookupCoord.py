###-------------------------------------------------------------------
### File    : lookupCoord.py
### Authors : Brian L. Troutwine <brian@troutwine.us>
###           Jared T. Sund      <jaredsund@gmail.com>
###           Cameron Kidd       <cameron.kidd@gmail.com>
### Description : This class calculates and stores the angular
###               positions for a provided planet incremented
###               in 1 arc-length
###
### Copyright (c) 2010 Brian L. Troutwine, Jared T. Sund, Cameron Kidd
### This code is licensed under the MIT license, see distributed copy.
###-----------------------------------------------------------
import math

class lookupCoord:

   ## ##--------------------------------------------------------------------
   ## ## Function: constructor.
   ## ## Description: initialize the class instant, input: ring info 
   ## ##--------------------------------------------------------------------
    def __init__(self, Xcenter, Ycenter, Radius, Angle):
	self.Xcenter = Xcenter
	self.Ycenter = Ycenter
	self.Radius = Radius
	self.Angle = Angle
	self.Circum = math.pi * 2 * Radius
	self.CIndex = math.ceil(self.Circum)  
    	self.firstPosition = round((self.Angle * self.Circum)/360)
	self.coords = {} 
	self.__buildList()
   ## - end of constructor

   ## ##--------------------------------------------------------------------
   ## ## Function: private buildList function
   ## ## Description: populate a dictionary with all arc length whole
   ## ##              number increments (Xpos, Ypos)
   ## ##--------------------------------------------------------------------
    def __buildList(self):
	for i in range(int(self.CIndex)):
	   Theta = (i*360)/self.Circum
	   Xstep = math.cos(math.radians(Theta))*self.Radius
	   Ystep = math.sin(math.radians(Theta))*self.Radius
	   if self.coords.has_key(i) == False:
	      self.coords[i] = (i,round(Xstep+self.Xcenter),round(Ystep+self.Ycenter), round(Theta))
	return
   ## - end of buildList function

   ## ##--------------------------------------------------------------------
   ## ## Function: getLocation(arcInt)
   ## ## Description: returns the Xpos,Ypos for a given arc length increment 
   ## ##--------------------------------------------------------------------
    def getLocation(self, arcInt):
	arcInt = arcInt % self.CIndex
	if self.coords.has_key(arcInt):
		return self.coords[arcInt]
	else:
	 	return arcInt 
   ## - end of getLocation function 

   ## ##--------------------------------------------------------------------
   ## ## Function: getList
   ## ## Description: returns the dictionary 
   ## ##--------------------------------------------------------------------
    def getList(self):
	return self.coords
   ## - end of getList function 


