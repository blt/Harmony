import unittest, zmq, struct
from struct   import pack, unpack
from harmonyd import ADD_STAR, ADD_PLANET, DEL_STAR, DEL_PLANET
from harmonyd import OKAY, ENOSTAR, ENOPLANET, ECRANKY

ADD_STAR   = int('00', 2)
ADD_PLANET = int('01', 2)
DEL_STAR   = int('11', 2)
DEL_PLANET = int('10', 2)

OKAY       = int('00', 2)
ENOSTAR    = int('01', 2)
ENOPLANET  = int('11', 2)
ECRANKY    = int('10', 2)

byteSize = struct.calcsize('B')

class NoStar(Exception):   pass
class NoPlanet(Exception): pass

class HarmonyTest(unittest.TestCase):
    universe = {}

    def setUp(self):
        ctx = zmq.Context()
        self.sck = ctx.socket(zmq.REQ)
        self.sck.connect("tcp://127.0.0.1:5000")
        self.pub = ctx.socket(zmq.SUB)
        self.pub.connect("tcp://127.0.0.1:5001")
        self.pub.setsockopt(zmq.SUBSCRIBE, "")

    def add_star(self, xpos, ypos):
        self.sck.send(pack('!BxQQ', ADD_STAR, xpos, ypos))
    def del_star(self, star_id):
        self.sck.send(pack('!BxQ', DEL_STAR, star_id))
    def add_planet(self, star_id, ang, spd, rad):
        self.sck.send(pack('!BxQHBH', ADD_PLANET, star_id, ang, spd, rad))
    def del_planet(self, star_id, planet_id):
        self.sck.send(pack('!BxQQ', DEL_PLANET, star_id, planet_id))

    def decode_hrm(self, msg):
        msg_id = unpack('!B', msg[0:byteSize])[0]
        if msg_id == ENOPLANET:
            raise NoPlanet
        elif msg_id == ENOSTAR:
            raise NoStar
        elif msg_id == OKAY:
            obj_id = unpack('!BxQ', msg)[1]
            return (OKAY, obj_id)
        else:
            raise NotImplemented
    def recv(self):
        return self.decode_hrm(self.sck.recv())

    def test_pubsup(self):
        raise NotImplemented

    def test_reqrep(self):
        # Add star. Should respond with (OKAY, star_id) and
        # then show up in the world broadcast.
        self.add_star(100, 100)
        (resp, star_id) = self.recv()
        self.assertEqual(resp, OKAY)
        # Delete just created star.
        self.del_star(star_id)
        (resp, rsid) = self.recv()
        self.assertEqual(resp, OKAY)
        self.assertEqual(rsid, star_id)
        # Add star again.
        self.add_star(100, 100)
        (resp, star_id) = self.recv()
        self.assertEqual(resp, OKAY)
        # Place a planet around it.
        self.add_planet(star_id, 10, 10, 10)
        (resp, planet_id) = self.recv()
        self.assertEqual(resp, OKAY)
        # Delete that planet!
        self.del_planet(star_id, planet_id)
        (resp, rpid) = self.recv()
        self.assertEqual(resp, OKAY)
        self.assertEqual(rsid, planet_id)
        # Add the planet back.
        self.add_planet(star_id, 10, 10, 10)
        (resp, planet_id) = self.recv()
        self.assertEqual(resp, OKAY)
        # Delete star.
        self.del_star(star_id)
        (resp, rsid) = self.recv()
        self.assertEqual(resp, OKAY)
        self.assertEqual(rsid, star_id)

if __name__ == "__main__":
    unittest.main()
