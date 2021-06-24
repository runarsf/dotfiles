#from qutebrowser.config import config

# Load existing settings made via :set
config.load_autoconfig()

import themes.dracula as draw
draw.blood(c, {
    'spacing': {
        'vertical': 6,
        'horizontal': 8
    }
})
#config.source('themes/nord.py')

# /usr/share/qutebrowser/scripts/dictcli.py install nb-NO
c.spellcheck.languages = ["en-US", "nb-NO"]

c.content.headers.user_agent = "Mozilla/5.0 (Windows NT 10.0; rv:68.0) Gecko/20100101 Firefox/68.0"

c.url.default_page = "https://google.com/"
c.url.start_pages = c.url.default_page

c.url.searchengines = {"DEFAULT": "https://google.com/search?q={}", "ddg": "https://duckduckgo.com/?q={}"}

c.colors.webpage.darkmode.enabled = True

c.editor.command = ["nvim", "-f", "{file}", "-c", "normal {line}G{column0}l"]

c.scrolling.bar = "overlay"
c.scrolling.smooth = True

c.window.hide_decoration = True

c.window.title_format = "{perc}{current_title}"

c.bindings.commands = {
    "normal": {
        "<Ctrl+,>": "set",
        "<Ctrl+f>": "set-cmd-text /",
        "<Ctrl+o>": "back",
        "gh": "home"
    }
}
