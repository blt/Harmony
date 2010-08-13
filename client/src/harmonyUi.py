import pygame, os
from pygame import Surface


# a uiSprite is any object that is drawn 
# onto the HarmonyUi surface.  This includes
# text fields and mutable fields.
class uiSprite(pygame.sprite.Sprite):
    image_path = None
    image = None
    rect = None
    id = None

    def __init__(self, imagefile, pos, type, id):
        pygame.sprite.Sprite.__init__(self)  
        if type == 'label':
             self.image_path = os.path.join(os.environ["HARMONY_MEDIA_DIR"],
             			    imagefile)
            # self.image_path = os.path.join('../media', imagefile)
             self.image = pygame.image.load(self.image_path).convert()
        elif type == 'field':
             font = pygame.font.Font(None, 25)
             self.image = font.render(imagefile, 1, (0,0,255))

        self.image.set_colorkey(self.image.get_at((0,0)), pygame.RLEACCEL)
        self.rect = self.image.get_rect(topleft=pos)
        self.id = id
    
        


class HarmonyUi(object):
    image = None
    rect = None
    currPos = (0, 0) # current position of mouse
    selKey = 'None' # key of selected star system
    keys = ['A','A#','B','C',
            'C#','D','D#','E', # the key of the next
            'F','F#','G','G#'] # star created
    keyIndex = 0
    editKey = None
    editSpeed = 1 # Speed of next planet placed
    selStarId = None # id of current selected star
    selPlanetId = None # id of selected planet
    selNote = None # note of current selecte planet
    selSpeed = None # speed of current selected planet 
    labelSprites = None # unmutable labels for ui fields
    fieldSprites = None # mutable ui fields
    clearBg = None # background to clear sprites
    editMode = 0 # if 1 show ui

    def __init__(self, screen, pos):
	self.image = Surface((screen.get_width(), 200))
        self.rect = self.image.get_rect()
        self.rect.topleft = (0, screen.get_height() - 200)
        self.currPos = pos
        self.editKey = self.keys[self.keyIndex]
        self.labelSprites = pygame.sprite.RenderUpdates()
        self.fieldSprites = pygame.sprite.RenderUpdates()
        self.clearBg = Surface(self.image.get_size())
        self.create_labels()
        self.create_fields()

    def set_editMode(self):
	if self.editMode:
	    self.editMode = 0
        else:
            self.editMode = 1

    def set_selected_star(self, star):
        self.selKey = star.key
        self.selStarId = star.sid

    def set_selected_planet(self, planet):
        self.selNote = planet.note
        self.selSpeed = planet.speed
	print self.selPlanetId
        self.selPlanetId = planet.pid
  
    def set_current_pos(self, pos):
	self.currPos = pos
    
    def get_edit_speed(self):
        return self.editSpeed

    def get_edit_key(self):
        return self.keyIndex

    def inc_edit_key(self):
	self.keyIndex += 1
	if self.keyIndex > 11:
	    self.keyIndex = 0
	
	self.editKey = self.keys[self.keyIndex]

    def inc_edit_speed(self):
	self.editSpeed += 1
	if self.editSpeed > 5:
	    self.editSpeed = 0

    # Create mutable fields and add them to 
    # the fieldSprites group. Since fields 
    # may change with each tick the group
    # is emptied and repopulated every call.
    def create_fields(self):
        self.fieldSprites.clear(self.image, self.clearBg)
        self.fieldSprites.empty()

        xpos = uiSprite(`self.currPos[0]`, (205,101), 'field', 'xpos')
        self.fieldSprites.add(xpos)
        ypos = uiSprite(`self.currPos[1]`, (205,126), 'field', 'ypos')
        self.fieldSprites.add(ypos)
        eSpeed = uiSprite(`self.editSpeed`, (80,100), 'field', 'eSpeed')
        self.fieldSprites.add(eSpeed)
        eKey = uiSprite(self.editKey, (80,125), 'field', 'eKey')
        self.fieldSprites.add(eKey)
        sid = uiSprite(`self.selStarId`, (353,101), 'field', 'sid')
        self.fieldSprites.add(sid)
        skey = uiSprite(self.selKey, (353,126), 'field', 'skey')
        self.fieldSprites.add(skey)
        pid = uiSprite(`self.selPlanetId`, (535,101), 'field', 'pid')
        self.fieldSprites.add(pid)
        pSpeed = uiSprite(`self.selSpeed`, (535,126), 'field', 'pSpeed')
        self.fieldSprites.add(pSpeed)

    # Create all of the labels for the ui fields
    # and place them in the labelSprites group
    def create_labels(self):
        xposLabel = uiSprite('xposlabel.png', (150, 100), 'label', 'xpl')
        self.labelSprites.add(xposLabel)
        yposLabel = uiSprite('yposlabel.png', (150, 125), 'label', 'ypl')
        self.labelSprites.add(yposLabel)
        speedLabel = uiSprite('speedlabel.png', (24, 100), 'label', 'spl')
        self.labelSprites.add(speedLabel)
        keyLabel = uiSprite('keylabel.png', (39, 125), 'label', 'kyl')
        self.labelSprites.add(keyLabel)
        staridLabel = uiSprite('staridlabel.png', (280, 100), 'label', 'sidl')
        self.labelSprites.add(staridLabel)
        starkeyLabel = uiSprite('starkeylabel.png', (275, 125), 'label', 'skl')
        self.labelSprites.add(starkeyLabel)
        pidLabel = uiSprite('planetidlabel.png', (448, 100), 'label', 'pidl')
        self.labelSprites.add(pidLabel)
        pspeedLabel = uiSprite('pslabel.png', (425, 125), 'label', 'psl')
        self.labelSprites.add(pspeedLabel)
 
    def draw_ui(self):
	self.labelSprites.clear(self.image, self.clearBg)
	if self.editMode:
            self.labelSprites.draw(self.image)
            self.fieldSprites.draw(self.image)
