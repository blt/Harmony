from pygame import image, Surface
import os, os.path, pygame

class Star(pygame.sprite.Sprite):
    image_pth = os.path.join(os.environ["HARMONY_MEDIA_DIR"],
                             "sun.xcf")
   # image_pth = os.path.join('../media', "sun.xcf")
    image      = None
    rect       = None
    vertx      = 0
    verty      = 0
    sid        = 0

    def __init__(self, pos, id):
        pygame.sprite.Sprite.__init__(self)
        self.vertx = pos[0]
        self.verty = pos[1]
        self.sid = id
	self.load_image()    

    def load_image(self):
        self.image = image.load(self.image_pth).convert()
        self.rect = self.image.get_rect(center = (self.vertx, self.verty))
        self.image.set_colorkey(self.image.get_at((0,0)), pygame.RLEACCEL)




            
    
