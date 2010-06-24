# Copyright (c) 2010 Brian L. Troutwine
#
# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated
# documentation files (the "Software"), to deal in the
# Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute,
# sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#
# The above copyright notice and this permission notice shall
# be included in all copies or substantial portions of the
# Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
# KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
# WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
# PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS
# OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
# OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
# OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

import zmq, time, struct

context = zmq.Context()
socket = context.socket(zmq.PUB)
socket.bind("tcp://127.0.0.1:5000")

def naturals(start):
    "Yield an infinite list of natural numbers."
    idx = start
    while True:
        yield idx
        idx += 1

def primes():
    """Differentiates integers as either prime or compound.

    Return value is a 2-tuple. The first element is a string,
    either "compound" or "prime", denoting the nature of the
    second element.
    """
    # This is a really silly prime finder.
    primes_lst = [2]
    for n in naturals(3):
        compound = False
        for p in primes_lst:
            if (n % p) == 0:
                compound = True
                break
        if compound == False:
            yield ('prime', n)
            primes_lst.append(n)
        else:
            yield ('compound', n)

if __name__ == "__main__":
    # Payload is encoded as simple C struct. Here's the
    # breakdown of the pieces:
    #
    # ! - Payload is in network byte order.
    # B - First element of payload is unsigned char: second
    #     element is a compound integer if 0, else if
    #     prime 1.
    # x - Pad byte, allows zeromq subscription.
    # Q - Second element of payload is unsigned long long:
    #     the prime or compound being sent over the wire.
    fmt = "!BQ"

    for z in primes():
        strt = None
        if z[0] == 'compound':
            strt = struct.pack(fmt, 0, z[1])
        else:
            strt = struct.pack(fmt, 1, z[1])
        socket.send(strt)
