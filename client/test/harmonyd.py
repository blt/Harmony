import zmq, sys, base64
try:
    import json
except ImportError:
    import simplejson as json


class Ticker(object):
    def tick(self):
        raise NotImplemented
    def serialize(self):
        raise NotImplemented

class Planet(Ticker):
    angle  = None
    speed  = None
    radius = None

    def __init__(self, angle=0, speed=1, radius=2):
        self.angle  = angle
        self.speed  = speed
        self.radius = radius

    def tick(self):
        # NOTE: This kind of sucks.
        self.angle = (self.angle + self.speed)%360

    def serialize(self):
        return (self.angle, self.speed, self.radius)

class Star(object):
    pos     = None
    planets = None

    def planet_id(self):
        i = 0
        while True:
            yield i
            i += 1

    def __init__(self, pos=(0,0)):
        self.pid_gen = self.planet_id()
        self.pos = pos
        self.planets = {}

    def add_planet(self, angle, speed, radius):
        pid = self.pid_gen.next()
        self.planets[pid] = Planet(angle, speed, radius)
        return pid

    def del_planet(self, pid):
        try:
            del self.planets[pid]
        except KeyError:
            raise NoPlanet

    def serialize(self):
        serl = (len(self.planets),
                [(planet_id, planet.serialize()) for
                 (planet_id, planet) in self.planets.iteritems()])
        return serl

    def tick(self):
        for p in self.planets.values():
            p.tick()

class Universe(Ticker):
    stars = None
    form    = '!Q'

    def star_id(self):
        i = 0
        while True:
            yield i
            i += 1

    def __init__(self):
        self.sid_gen = self.star_id()
        self.stars = {}

    def add_star(self, pos):
        sid = self.sid_gen.next()
        self.stars[sid] = Star(pos)
        return sid

    def del_star(self, sid):
        try:
            del self.stars[sid]
        except KeyError:
            raise NoStar

    def add_planet(self, sid, angle, speed, radius):
        try:
             return self.stars[sid].add_planet(angle, speed, radius)
        except KeyError:
            raise NoStar

    def del_planet(self, sid, pid):
        try:
            self.stars[sid].del_planet(pid)
        except KeyError:
            raise NoPlanet

    def __str__(self):
        return "<%d, [%s]>" % (len(self.stars), ''.join([str(s) for s in self.stars]))

    def serialize(self):
        serl = (len(self.stars),
                [(star_id, star.serialize()) for
                 (star_id, star) in self.stars.iteritems()])
        return serl

    def tick(self):
        for s in self.stars.values():
            s.tick()

class NoStar(Exception):   pass
class NoPlanet(Exception): pass

ADD_STAR   = int('00', 2)
ADD_PLANET = int('01', 2)
DEL_STAR   = int('11', 2)
DEL_PLANET = int('10', 2)

OKAY       = int('00', 2)
ENOSTAR    = int('01', 2)
ENOPLANET  = int('11', 2)
ECRANKY    = int('10', 2)

def recv(sock):
    msg = sock.recv()
    return json.loads(base64.b64decode(msg))
def send(sock, msg):
    msg = base64.b64encode(json.dumps(msg))
    sock.send(msg)

def main():
    ctx = zmq.Context()
    s = ctx.socket(zmq.REP)
    s.bind("tcp://127.0.0.1:5000")
    pub = ctx.socket(zmq.PUB)
    pub.bind("tcp://127.0.0.1:5001")

    u = Universe()
    while True:
        u.tick()
        msg = recv(s)
        msg_id = msg[0]
        try:
            if msg_id == ADD_STAR:
                xpos, ypos = msg[1:]
                ret_id = u.add_star((xpos, ypos))
            elif msg_id == DEL_STAR:
                star_id = msg[1]
                u.del_star(star_id)
                ret_id = star_id
            elif msg_id == ADD_PLANET:
                star_id, angle, speed, radius = msg[1:]
                ret_id = u.add_planet(star_id, angle, speed, radius)
            elif msg_id == DEL_PLANET:
                star_id, planet_id = msg[1:]
                u.del_planet(star_id, planet_id)
                ret_id = planet_id
            else:
                raise NotImplemented
        except NoPlanet:
            send(s, ENOPLANET)
        except NoStar:
            send(s, ENOSTAR)
        else:
            send(s, (OKAY, ret_id))
        send(pub, u.serialize())

if __name__ == "__main__":
    main()
