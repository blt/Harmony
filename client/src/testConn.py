###-------------------------------------------------------------------
### File    : testConn.py
### Authors : Brian L. Troutwine <brian@troutwine.us>
###           Jared T. Sund      <jaredsund@gmail.com>
###           Cameron Kidd       <cameron.kidd@gmail.com>
### Description : test file
###
### Copyright (c) 2010 Brian L. Troutwine, Jared T. Sund, Cameron Kidd

import harmonyConn 
from harmonyConn import *

import lookupCoord
from lookupCoord import *


#x = harmonyConn('sundhome.com', 1234)
x = harmonyConn('127.0.0.1', 1234)

#error, invalid command
print "Add Error", x.addError(33,44)

#test add star
print "Add Star", x.addStar(32,43,5)

#test delete star
print "Del Star", x.delStar(46)

#test add planet
print "Add Planet", x.addPlanet(23, 43, 23, 23, 2)

#test delete planet
print "Del Planet ", x.delPlanet(32, 43)

#test get universe
print "getUNI ", x.getUNI(1)



###-----------------------------------------------------------
###-----------------------------------------------------------
###-----------------------------------------------------------
###-----------------------------------------------------------

#lookup(XCenter, YCenter, Radius, Angle)
c = lookupCoord(0,0,10,270)

#get the first arclength position based on the 
    #planets startup angle
firstP = c.firstPosition
print firstP

#getLocation(arclength location)
for i in range(int(firstP), 63):
	print c.getLocation(i)

#getList -- returns the coordinates dictionary
#print c.getList()

