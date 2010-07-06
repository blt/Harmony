import unittest, zmq, base64
try:
    import json
except ImportError:
    import simplejson as json
from harmonyd import ADD_STAR, ADD_PLANET, DEL_STAR, DEL_PLANET
from harmonyd import OKAY, ENOSTAR, ENOPLANET, ECRANKY
from harmonyd import NoStar, NoPlanet

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
        self.send(self.sck, (ADD_STAR, xpos, ypos))
    def del_star(self, star_id):
        self.send(self.sck, (DEL_STAR, star_id))
    def add_planet(self, star_id, ang, spd, rad):
        self.send(self.sck, (ADD_PLANET, star_id, ang, spd, rad))
    def del_planet(self, star_id, planet_id):
        self.send(self.sck, (DEL_PLANET, star_id, planet_id))

    def recv(self, sock):
        msg = sock.recv()
        return json.loads(base64.b64decode(msg))
    def send(self, sock, msg):
        msg = base64.b64encode(json.dumps(msg))
        sock.send(msg)

    def test_pubsub(self):
        # Add a star.
        self.add_star(100, 100)
        (resp, star_id) = self.recv(self.sck)
        self.assertEqual(resp, OKAY)
        # Ensure that it shows up in the PUB feed.
        wrld = self.recv(self.pub)
        self.assertEqual(wrld[0], 1)
        self.assertEqual(wrld[1][0][0], star_id)
        # Delete star.
        self.del_star(star_id)
        (resp, rsid) = self.recv(self.sck)
        self.assertEqual(resp, OKAY)
        self.assertEqual(rsid, star_id)

    def test_reqrep(self):
        # Add star. Should respond with (OKAY, star_id) and
        # then show up in the world broadcast.
        self.add_star(100, 100)
        (resp, star_id) = self.recv(self.sck)
        self.assertEqual(resp, OKAY)
        # Delete just created star.
        self.del_star(star_id)
        (resp, rsid) = self.recv(self.sck)
        self.assertEqual(resp, OKAY)
        self.assertEqual(rsid, star_id)
        # Add star again.
        self.add_star(100, 100)
        (resp, star_id) = self.recv(self.sck)
        self.assertEqual(resp, OKAY)
        # Place a planet around it.
        self.add_planet(star_id, 10, 10, 10)
        (resp, planet_id) = self.recv(self.sck)
        self.assertEqual(resp, OKAY)
        # Delete that planet!
        self.del_planet(star_id, planet_id)
        (resp, rpid) = self.recv(self.sck)
        self.assertEqual(resp, OKAY)
        self.assertEqual(rpid, planet_id)
        # Add the planet back.
        self.add_planet(star_id, 10, 10, 10)
        (resp, planet_id) = self.recv(self.sck)
        self.assertEqual(resp, OKAY)
        # Delete star.
        self.del_star(star_id)
        (resp, rsid) = self.recv(self.sck)
        self.assertEqual(resp, OKAY)
        self.assertEqual(rsid, star_id)

if __name__ == "__main__":
    unittest.main()
