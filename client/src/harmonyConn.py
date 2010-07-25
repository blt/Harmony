###-----------------------------------------------------------
### File 	: harmonyConn.py
### Author:	: Jared T. Sund <jaredsund@gmail.com>>
### Description : python class for sending information to the
###		  harmony erlang server
###
### Created 	: 23 Jul 2010 by Jared T. Sund <jaredsund@gmail.com>
###-----------------------------------------------------------
import socket
import struct
from struct import *

class harmonyConn:

   def __init__(self,server,port):
       	self.pckUnpack = '!BI'
	self.bufSize = 1024
	self.UNIbufSize = 1024
	self.server = server #'127.0.0.1'
	self.port = port #1234


   def makeConnection(self):
	s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	s.connect((self.server, self.port))
	return s

   def addError(self,Xpos, Ypos):
	s = self.makeConnection()
	p = struct.pack('!BHH',10,Xpos,Ypos)
	s.send(p)
	data = s.recv(self.bufSize)
	s.close()
	output = struct.unpack(self.pckUnpack, data)
    	return output

   def addStar(self,Xpos, Ypos):
	s = self.makeConnection()
	p = struct.pack('!BHH',1,Xpos,Ypos)
	s.send(p)
	data = s.recv(self.bufSize)
	s.close()
	output = struct.unpack(self.pckUnpack, data)
    	return output

   def delStar(self,StarId):
	s = self.makeConnection()
	p = struct.pack('!BI',2,StarId)
	s.send(p)
	data = s.recv(self.bufSize)
	s.close()
	output = struct.unpack(self.pckUnpack, data)
    	return output

   def addPlanet(self,StarId, Angle, Speed, Radius):
	s = self.makeConnection()
	p = struct.pack('!BIHHH',4,StarId,Angle,Speed,Radius)
	s.send(p)
	data = s.recv(self.bufSize)
	s.close()
	output = struct.unpack(self.pckUnpack, data)
    	return output

   def delPlanet(self,StarId, PlanetId):
	s = self.makeConnection()
	p = struct.pack('!BII',8,StarId,PlanetId)
	s.send(p)
	data = s.recv(self.bufSize)
	s.close()
	output = struct.unpack(self.pckUnpack, data)
    	return output

   def getUNI(self):
	s = self.makeConnection()
	p = struct.pack('!B',16)
	s.send(p)
	data = s.recv(self.UNIbufSize)
	s.close()

	openState = '!BIIIH' #8,32,32,32,16: success/failure, megsec,sec,microsec,#stars
	starInfo = '!IHHH'  #32,16,16,16: starId, XPos, YPos, #planets
	planetInfo = '!IHHH' #32,16,16,16: planetId, angle, speed, radius

	start = 0
	next = calcsize(openState)

	success, megSec, sec, micSec,numStars = struct.unpack(openState, data[start:next])
	time = (megSec,sec,micSec)
	#print "get UNI "
	#print "universe ", i, time, numStars

	start = next
	next += calcsize(starInfo)
	stars = list()
	for i in range(numStars):
	  starId,X,Y,numPlanets = struct.unpack(starInfo,data[start:next])
	  star= (starId,X,Y)
	  #print "star ", star, numPlanets
	  start = next
	  next+= calcsize(starInfo)
	  planets = list()
	  for j in range(numPlanets):
		planetId, speed, angle, radius = struct.unpack(planetInfo, data[start:next])
		planet = ("planet",planetId, speed, angle, radius)
		#print "planet ", planet
		start = next
		next += calcsize(planetInfo)
		planets.append(planet)
	  stars.append(("system", star, planets))

	output = (success, ("universe", time, stars))
	return  output 
