from collections import deque
from harmonyConn import harmonyConn

## The UpdateQ class queues all universe 
## updates from the network server.
  
class UpdateQ(object):

    updateQ = None
    hConn = None
    timestamp = None
    timer = 0
    del_flag = 0

    def __init__ (self, hc):
        self.timestamp = 0 
        self.updateQ = deque()
        self.hConn = hc #Jared's conn object
	self.timer = 0

    # Checks for update from server.
    # If first index of update package is
    # 1, then new is data available, and 
    # method returns 1. 
    def check_for_update(self):
        if self.timer == 0:
            upkg = self.hConn.getUNI(0)
	    self.del_flag = 1
	else:
            upkg = self.hConn.getUNI(int(self.timestamp))
            self.del_flag = 0
       
	self.timer += 1
        if self.timer > 10:
	   self.timer = 0
        
	if upkg[0] == 1:
            self.updateQ.append(upkg[1])
            self.timestamp = upkg[1][1] 
            return 1
        else:
            return 0


    # front of queue
    def get_next_update(self):
        try:
 	    return self.updateQ.popleft()
        except IndexError:
            print "Error: Accessing an empty update queue"
            return 0         
   
