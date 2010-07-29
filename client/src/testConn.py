###-----------------------------------------------------------
### File 	: test.py
### Author:	: Jared T. Sund <jaredsund@gmail.com>>
### Description :
###
### Created 	: 23 Jul 2010 by Jared T. Sund <jaredsund@gmail.com>
###-----------------------------------------------------------
import harmonyConn 
from harmonyConn import *

import lookupCoord
from lookupCoord import *


x = harmonyConn('sundhome.com', 1234)
#x = harmonyConn('127.0.0.1', 1234)

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

#test location
print "location ", x.location(32)



#lookup(XCenter, YCenter, Radius)
c = lookupCoord(0,0,10)

#getLocation(arclength location)
for i in range(163):
	print c.getLocation(i)

#getList -- returns the coordinates dictionary
#print c.getList()

