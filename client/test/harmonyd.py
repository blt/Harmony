import zmq, sys, struct
from struct import pack, unpack

class Ticker(object):
    form = None

    def tick(self):
        raise NotImplemented

    def serialize(self):
        raise NotImplemented

class Planet(Ticker):
    angle  = None
    speed  = None
    radius = None
    form   = '!HBH'

    def __init__(self, angle=0, speed=1, radius=2):
        self.angle  = angle
        self.speed  = speed
        self.radius = radius

    def tick(self):
        # NOTE: This kind of sucks.
        self.angle = (self.angle + self.speed)%360

    def serialize(self):
        return pack(self.form, self.angle, self.speed, self.radius)

class Star(object):
    pos     = None
    planets = {}
    # Star serialized have as leader unsigned long long giving
    # number of planets, which is followed by constructed
    # serialization of all planets. Format is this:
    #
    # Qx[QHBH]
    #
    # That is:
    # NUM_PLANETS:64 PAD:8 PLANET_ID:64 ANGLE:16 SPEED:8
    # Q              x     Q            H        B
    #
    # RADIUS:16
    # H
    form    = '!Qx'

    def planet_id(self):
        i = 0
        while True:
            yield i
            i += 1

    def __init__(self, pos=(0,0)):
        self.pid_gen = self.planet_id()
        self.pos = pos

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
        serl = [pack(self.form, len(self.planets))]
        for pk, pv in self.planets.iteritems():
            serl.append(pack('!Q', pk) + pv.serialize())
        return ''.join(serl)

    def tick(self):
        for p in self.planets.values():
            p.tick()

class Universe(Ticker):
    stars = None
    # Universe serialized have as leader unsigned long long
    # giving number of stars, which is followed by
    # constructed serialization of all stars. Format is
    # this:
    #
    # Qx[Q$STAR]
    #
    # That is:
    # NUM_PLANETS:64 PAD:8 STAR_ID:64 FOO
    form    = '!Qx'

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

    def serialize(self):
        serl = [pack(self.form, len(self.stars))]
        for sk, sv in self.stars.iteritems():
            serl.append(pack('!Q', sk) + sv.serialize())
        return ''.join(serl)

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

byteSize = struct.calcsize('B')

def main():
    ctx = zmq.Context()
    s = ctx.socket(zmq.REP)
    s.bind("tcp://127.0.0.1:5000")
    pub = ctx.socket(zmq.PUB)
    pub.bind("tcp://127.0.0.1:5001")

    u = Universe()
    while True:
        u.tick()
        msg = s.recv()
        msg_id = unpack('!B', msg[0:byteSize])[0]
        try:
            if msg_id == ADD_STAR:
                # MSG_ID:8, XPOS:64, YPOS:64
                # B       x Q        Q
                xpos, ypos = unpack('!BxQQ', msg)[1:]
                ret_id = u.add_star((xpos, ypos))
            elif msg_id == DEL_STAR:
                # MSG_ID:8, STAR_ID:64
                # B       x Q
                star_id = unpack('!BxQ', msg)[1]
                u.del_star(star_id)
                ret_id = star_id
            elif msg_id == ADD_PLANET:
                # MSG_ID:8, STAR_ID:64, ANGLE:16, SPEED:8, RADIUS:16
                # B       x Q           H         B        H
                star_id, angle, speed, radius = unpack('!BxQHBH', msg)[1:]
                ret_id = u.add_planet(star_id, angle, speed, radius)
            elif msg_id == DEL_PLANET:
                # MSG_ID:8, STAR_ID:64, PLANET_ID:64
                # B       x Q           Q
                star_id, planet_id = unpack('!BxQQ', msg)[1:]
                u.del_planet(star_id, planet_id)
                ret_id = planet_id
            else:
                raise NotImplemented
        except NoPlanet:
            s.send(pack('!B', ENOPLANET))
        except NoStar:
            s.send(pack('!B', ENOSTAR))
        else:
            s.send(pack('!BxQ', OKAY, ret_id))
        pub.send(u.serialize())

if __name__ == "__main__":
    main()
