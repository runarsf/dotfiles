#from qutebrowser.config import config

# Load existing settings made via :set
config.load_autoconfig()

#import themes.dracula as draw
#draw.blood(c, {
#    'spacing': {
#        'vertical': 6,
#        'horizontal': 8
#    }
#})
config.source('themes/nord.py')
