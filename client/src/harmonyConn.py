###-------------------------------------------------------------------
### File    : harmonyConn.py
### Authors : Brian L. Troutwine <brian@troutwine.us>
###           Jared T. Sund      <jaredsund@gmail.com>
###           Cameron Kidd       <cameron.kidd@gmail.com>
### Description : python class for sending information to the
###		  harmony erlang server
###
### Copyright (c) 2010 Brian L. Troutwine, Jared T. Sund, Cameron Kidd
### This code is licensed under the MIT license, see distributed copy.
###-------------------------------------------------------------------

import socket
import struct


## ##--------------------------------------------------------------------
## ## Class: harmonyConn 
## ## Description: Harmony network client functions
## ##--------------------------------------------------------------------
class harmonyConn:

   ## ##--------------------------------------------------------------------
   ## ## Function: constructor.
   ## ## Description: initialize the class instant, input: server, and port 
   ## ##--------------------------------------------------------------------
   def __init__(self,server,port):
       	self.pckUnpack = '!BH' 	#general unpack, 8 and 16 bit
	self.commandState = '!B'
	self.bufSize = 1024 	#response buffer size
	self.UNIbufSize = 1024 	#buffer size for universe response
	self.server = server 	#'127.0.0.1'
	self.port = port 	#1234
   ## - end of constructor

   ## ##--------------------------------------------------------------------
   ## ## Function: makeConnection 
   ## ## Description: returns a new connected socket
   ## ##--------------------------------------------------------------------
   def makeConnection(self):
	s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	s.connect((self.server, self.port))
	return s
   ## - end of makeConnection function
   
   ## ##--------------------------------------------------------------------
   ## ## Function: checkError
   ## ## Description: checks the server return for an error (0,errorCode)
   ## ##--------------------------------------------------------------------
   def checkError(self, data):
	start = 0
	next = struct.calcsize('!B')
	state = struct.unpack('!B',data[start:next])
	if state == (0,):
	  return struct.unpack('!BB',data)
	else:
	  return
   ## - end of checkError function

   def conTime(Self, Meg, Sec, Mic):
       return  ((Meg * 1000000.0) + Sec + (Mic/1000000.0))


   ## ##--------------------------------------------------------------------
   ## ## Function: addStar 
   ## ## Description: makes a request to the server to add a new star
   ## ##   	      takes as input the X and Y position of the star
   ## ##              returns the function's success (0/1) and the new
   ## ##              starId
   ## ##--------------------------------------------------------------------
   def addStar(self, Xpos, Ypos, key):
	s = self.makeConnection()
	p = struct.pack('!BHHB',1,Xpos,Ypos,key)

	s.send(p)
	data = s.recv(self.bufSize)
	s.close()

    	output = self.checkError(data)
    	if output == None:
	   output = struct.unpack(self.pckUnpack,data)
	
    	return output
   ## - end of addStar function

   ## ##--------------------------------------------------------------------
   ## ## Function: delStar
   ## ## Description: makes a request to the server to del a current star
   ## ##              takes as input a star's ID, and returns the function's
   ## ##              success (0/1) and the star's ID
   ## ##--------------------------------------------------------------------
   def delStar(self,StarId):
	s = self.makeConnection()
	p = struct.pack('!BH',2,StarId)
	s.send(p)
	data = s.recv(self.bufSize)
	s.close()
    	output = self.checkError(data)
    	if output == None:
	   output = struct.unpack(self.pckUnpack, data)
    	return output
   ## - end of delStar function

   ## ##--------------------------------------------------------------------
   ## ## Function: addPlanet
   ## ## Description: makes a requeste to the server to add a planet to an
   ## ##              existing star.  Takes as input the star's ID, Angle,
   ## ##              Speed, and Radius.  Returns the function's success (0/1),
   ## ##              and the new planet's id
   ## ##--------------------------------------------------------------------
   def addPlanet(self, StarId, Angle, Speed, Radius, Note):
	s = self.makeConnection()
	p = struct.pack('!BHHHHB',4,StarId,Angle,Speed,Radius,Note)
	s.send(p)
	data = s.recv(self.bufSize)
	s.close()
    	output = self.checkError(data)
    	if output == None:
	   output = struct.unpack(self.pckUnpack, data)
    	return output
   ## - end of addPlanet function

   ## ##--------------------------------------------------------------------
   ## ## Function: delPlanet
   ## ## Description: makes a request to the server to delete a planet
   ## ##              takes as input the star's ID who the planet belongs
   ## ##              to, and the planet ID to be deleted.  Returns the
   ## ##              functions success (0/1), and the planet's ID
   ## ##--------------------------------------------------------------------
   def delPlanet(self,StarId, PlanetId):
	s = self.makeConnection()
	p = struct.pack('!BHH',8,StarId,PlanetId)
	s.send(p)
	data = s.recv(self.bufSize)
	s.close()
    	output = self.checkError(data)
    	if output == None:
	   output = struct.unpack(self.pckUnpack, data)
    	return output
   ## - end of delPlanet function


   ## ##--------------------------------------------------------------------
   ## ## Function: getUNI 
   ## ## Description: makes a request to the server, for all of the stars
   ## ##              and planets.
   ## ##--------------------------------------------------------------------
   def getUNI(self, clientTime):
   #def getUNI(self, (MegSec, Sec, MicSec)):
        
        MegSec = clientTime / 1000000
	Sec = clientTime - MegSec * 1000000
	MicSec = clientTime - int(clientTime)
        
        ## - make a connection to the server, issue command 16 (gen_UNI)
        ## -  recieve the response in variable data
	s = self.makeConnection()
	p = struct.pack('!BHII',16, MegSec, Sec, MicSec)
	s.send(p)
	data = s.recv(self.UNIbufSize)
	s.close()

    	output = self.checkError(data)
    	if output == None:
          ## - bit sizes for incomming data
	  openState = '!BHIIH' #success/failure, megsec,sec,microsec,#stars
	  starInfo = '!HHHBHIIH'  #starId, XPos, YPos, Key,megsec, sec,microsec, #planets
	  planetInfo = '!HHHHBHII' #planetId, angle, speed, radius, Note, megsec, sec, micsec

	  ## - bit counters for parsing the bitstring (unpack)
	  start = 0
	  next = struct.calcsize(openState)

    	  ## - pull the function's success (0/1), timestamp, and number of stars
          ## - from the bitstring
	  #success = struct.unpack(openState, data[start:next])
	  success, MegSec, Sec, MicroSec, numStars = struct.unpack(openState, data[start:next])

          ## - increment the bit counters for the bitstring
	  start = next
	  next += struct.calcsize(starInfo)

	  stars = list() # new list for holding the stars

	  ## - loop through and collect all the stars from the bitstring
	  for i in range(numStars):
	    starId,X,Y,key,sMegSec,sSec,sMicSec,numPlanets = struct.unpack(starInfo,data[start:next])

	    ## - increment the bit counters for the bitstring
	    start = next
	    next += struct.calcsize(starInfo)
          
	    planets = list() # new list for holding the current star's planet
	    ## - loop through and collect the planets for the current star
	    for j in range(numPlanets):
		planetId, angle,speed, radius, note, pMegSec, pSec, pMicSec = struct.unpack(planetInfo, data[start:next])
		## - add each planet to the current star's planet list
		planets.append( ("planet",planetId, angle,speed, radius, note, self.conTime(pMegSec,pSec,pMicSec)) ) #planet tuple
	        
		## - increment the bit counters for the bitstring
		start = next
		next += struct.calcsize(planetInfo)
	    ## - end of j loop (planets)
    	  
    	    ## - add the current star to the stars list
	    stars.append(("system", starId,X,Y,key,self.conTime(sMegSec,sSec,sMicSec), planets))
   	  ## - end of i loop (stars)

	  ## - create the final tuple 
	  output = (success, ("universe",self.conTime(MegSec, Sec, MicroSec),stars))

	return output
	## - end of get_uni function

