###-----------------------------------------------------------
### File 	: test.py
### Author:	: Jared T. Sund <jaredsund@gmail.com>>
### Description :
###
### Created 	: 23 Jul 2010 by Jared T. Sund <jaredsund@gmail.com>
###-----------------------------------------------------------

import harmonyConn 
from harmonyConn import *


x = harmonyConn('127.0.0.1', 1234)

#error, invalid command
f = x.addError(33,44)
print "Add Error", struct.unpack(x.pckUnpack, f)


#test add star
f = x.addStar(32,43)
print "Add Star ", struct.unpack(x.pckUnpack, f)

#test delete star
f = x.delStar(46)
print "Del Star ",struct.unpack(x.pckUnpack, f)

#test add planet
f = x.addPlanet(23, 43, 23, 23)
print "Add Planet ", struct.unpack(x.pckUnpack,f)

#test delete planet
f = x.delPlanet(32, 43)
print "Del Planet ", struct.unpack(x.pckUnpack,f)

#test get universe
f = x.getUNI()
print "get UNI ", struct.unpack(x.pckUnpack,f)

