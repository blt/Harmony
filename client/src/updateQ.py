from collections import deque
from harmonyConn import harmonyConn

## The UpdateQ class queues all universe 
## updates from the network server.
  
class UpdateQ(object):

    updateQ = None
    hConn = None
    timestamp = None

    def __init__ (self, hc):
        self.timestamp = (0,0)
        self.updateQ = deque()
        self.hConn = hc #Jared's conn object

    # Checks for update from server.
    # If first index of update package is
    # 1, then new is data available, and 
    # method returns 1. 
    def check_for_update(self):
        upkg = self.hConn.getUNI(1)
        if upkg[0] == 1:
             self.updateQ.append(upkg[2])
             self.timestamp = upkg[1] 
             return 1
        else:
             return 0

    # return the earliest update from
    # front of queue
    def get_next_update(self):
        try:
 	    return self.updateQ.popleft()
        except IndexError:
            print "Error: Accessing an empty update queue"
            return 0         
   
