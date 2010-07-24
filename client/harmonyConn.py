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
    	return data

   def addStar(self,Xpos, Ypos):
	s = self.makeConnection()
	p = struct.pack('!BHH',1,Xpos,Ypos)
	s.send(p)
	data = s.recv(self.bufSize)
	s.close()
    	return data

   def delStar(self,StarId):
	s = self.makeConnection()
	p = struct.pack('!BI',2,StarId)
	s.send(p)
	data = s.recv(self.bufSize)
	s.close()
	return data

   def addPlanet(self,StarId, Angle, Speed, Radius):
	s = self.makeConnection()
	p = struct.pack('!BIHHH',4,StarId,Angle,Speed,Radius)
	s.send(p)
	data = s.recv(self.bufSize)
	s.close()
	return data

   def delPlanet(self,StarId, PlanetId):
	s = self.makeConnection()
	p = struct.pack('!BII',8,StarId,PlanetId)
	s.send(p)
	data = s.recv(self.bufSize)
	s.close()
	return data

   def getUNI(self):
	s = self.makeConnection()
	p = struct.pack('!B',16)
	s.send(p)
	data = s.recv(self.bufSize)
	s.close()
	return data


