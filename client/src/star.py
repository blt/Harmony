from pygame import image
import os, os.path

class Star(object):
    image_pth = os.path.join(os.environ["HARMONY_MEDIA_DIR"],
                             "star.png")

    def __init__(self, pos):
        self.image = image.load(self.image_pth).convert()
        self.rect = self.image.get_rect(center = pos)
