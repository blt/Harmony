from pygame import image
import os, os.path, pygame

class Star(object):
    image_pth = os.path.join(os.environ["HARMONY_MEDIA_DIR"],
                             "star.png")
    image_r1pth = os.path.join(os.environ["HARMONY_MEDIA_DIR"],
                             "ring1.tga")
    image = None
    rect = None
    ring1 = None
    vertex = 0, 0
    rings = []

    def __init__(self, pos):
        self.image = image.load(self.image_pth).convert()
        self.rect = self.image.get_rect(center = pos)
        self.ring1 = image.load(self.image_r1pth).convert()
        self.ring1.set_colorkey(self.ring1.get_at((0,0)), pygame.RLEACCEL)
        self.vertex = pos
        self.rings.append(self.ring1)
        
