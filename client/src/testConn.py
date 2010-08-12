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


#test add star
#(X,starID) = x.addStar(1,2,3)
resp = (Conf, StarId) = x.addStar(1,2,3)
print "Add Star", resp
#print "Add Star", X, starID

#test delete star
#print "Del Star", x.delStar(0)

#test add planet
print "Add Planet", x.addPlanet(StarId,1,2,3,4)
#print "Add Planet", x.addPlanet(0,5,6,7,8)

#test delete planet
#print "Del Planet ", x.delPlanet(32, 43)

#test get universe
print "getUNI ", x.getUNI(0)



###-----------------------------------------------------------
###-----------------------------------------------------------
###-----------------------------------------------------------
###-----------------------------------------------------------

#lookup(XCenter, YCenter, Radius, Angle)
c = lookupCoord(0,0,10,270)

#get the first arclength position based on the
    #planets startup angle
#firstP = c.firstPosition
#print firstP

#getLocation(arclength location)
#for i in range(int(firstP), 3):
	#print c.getLocation(i)

#getList -- returns the coordinates dictionary
#print c.getList()

