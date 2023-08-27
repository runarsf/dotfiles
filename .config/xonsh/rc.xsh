# vim: set syntax=python foldmethod=marker:
from os.path import (realpath as _realpath, join as _join)
from shutil import which as _which
from xonsh.xontribs import xontribs_load as _xontribs_load
from xonsh.platform import ON_LINUX as _ON_LINUX
# from xonsh.aliases import source_foreign_fn

# Redirect output of python command to file
#   >>> echo @(__import__('inspect').getsource($PROMPT_FIELDS['gitstatus'])) >> FILE
# Inspec function
#   >>> __import__('inspect').getsource($PROMPT_FIELDS['gitstatus'])

# Helper function to join paths and get full path.
path = lambda *arg: _realpath(_join(*arg))

# Environment variables - https://xon.sh/envvars.html {{{

# Automatically continue disowned jobs
$AUTO_CONTINUE = True

# Select a tab completion result without executing the current line
$COMPLETIONS_CONFIRM = True

# Control what is saved to history
$HISTCONTROL = {'ignoredups', 'ignorespace'}

# Remove front dot in multiline input to make the code copy-pastable.
$MULTILINE_PROMPT = ' '

# Avoid typing cd just directory path.
$AUTO_CD = True

# Suppress the message that informs the user when a foreign alias has been skipped
# because it already exists in xonsh.
$FOREIGN_ALIASES_SUPPRESS_SKIP_MESSAGE = True

# Show a traceback when exceptions occur in the shell.
$XONSH_SHOW_TRACEBACK = True

# When enabled the prompt is rendered using threads.
$ENABLE_ASYNC_PROMPT = True

# The timeout (in seconds) for version control branch computations.
# This is a timeout per subprocess call,
# so the total time to compute will be larger than this in many cases.
$VC_BRANCH_TIMEOUT = 0.2

# Automatically push directories onto the directory stack.
$AUTO_PUSHD = True

# Sets whether completions should be case sensitive or case insensitive.
$CASE_SENSITIVE_COMPLETIONS = False

# The string used to show a shortened directory in a shortened cwd.
$DYNAMIC_CWD_ELISION_CHAR = '…'

# Places the auto-suggest result as the first option in the completions.
# This enables you to tab complete the auto-suggestion.
$AUTO_SUGGEST_IN_COMPLETIONS = True

# Auto-insert matching parentheses, brackets, and quotes.
# Only available under the prompt-toolkit shell.
$XONSH_AUTOPAIR = True
# }}}

# Aliases {{{
aliases['ls'] = ['ls', '-l', '-A', '-F', '-h']
if $(ls --version 2>/dev/null).strip():
    aliases['ls'].append('--color=auto')
if _which('exa') is not None:
    aliases['ls'] = ['exa', '--group-directories-first', '--git', '--long', '--all']

aliases['dk'] = ['docker']
aliases['dkc'] = aliases['dk'] + ['compose']
aliases['dkcl'] = aliases['dkc'] + ['logs']
aliases['dkcL'] = aliases['dkcl'] + ['-f']
aliases['dkcb'] = aliases['dkc'] + ['build']
aliases['dkcB'] = aliases['dkcb'] + ['--no-cache']
aliases['dkcd'] = aliases['dkc'] + ['down']
aliases['dkcu'] = aliases['dkc'] + ['up']
aliases['dkcU'] = aliases['dkcu'] + ['-d']
aliases['dkcUf'] = aliases['dkcU'] + ['--force-recreate', '--remove-orphans']
aliases['dkx']   = lambda args: $[docker exec -it @(args) sh]

@aliases.register(".")
@aliases.register("..")
@aliases.register("...")
@aliases.register("....")
def _supercomma():
    cd @("../" * len($__ALIAS_NAME))

aliases |= {
    '-': 'cd -',
    '..': 'cd ..',
}
# }}}

# Xontribs {{{
_xontribs = [
  'abbrevs',
  'readable-traceback',
  'argcomplete',
  'mpl',
  'jupyter',
]
# }}}

# JSON support {{{
try:
    import __builtin__
except ImportError:
    import builtins as __builtin__
__builtin__.true = True
__builtin__.false = False
__builtin__.null = None
# }}}

# Platform-specific settings {{{
if _ON_LINUX:
    # if which('zsh') is not None:
    #     source-foreign zsh --overwrite-aliases --interactive True --sourcer source 'echo Loading foreign shell zsh.'

    _xontribs = list(set([
        # 'coreutils',          # Additional core utilities that are implemented in xonsh.
        'fzf-widgets',        # Adds some fzf widgets.
        # 'zoxide',             # - curl -sS https://webinstall.dev/zoxide | bash
        'sh',                 # Paste and run commands from bash, zsh, fish in xonsh.
        'argcomplete',
        # 'docker_tabcomplete',
        'default_command',
    ] + _xontribs))

    def _git_bare(git_dir, work_tree, *args):
        if not args:
            args = ('status')
        git --git-dir=@(git_dir) --work-tree=@(work_tree) @(args)

    $DOTFILES_DIR = path(${'HOME'}, '.config/dotfiles/.git')
    $DOTFILES_TREE = path(${'HOME'})

    aliases['dotfiles']  = lambda args: _git_bare(${'DOTFILES_DIR'}, ${'DOTFILES_TREE'}, *args)
    aliases['dots']      = ['dotfiles']
    aliases['dirtydots'] = ['dirtygit', '--git-dir', ${'DOTFILES_DIR'}, '--work-tree', ${'DOTFILES_TREE'}, '--git-add', '-u']
    aliases['dotted']    = ['dotfiles', 'ls-files', '--error-unmatch']
    aliases['gitted']    = ['git', 'ls-files', '--error-unmatch']
    aliases['c']         = ['xclip', '-selection', 'clipboard']

    $fzf_history_binding = "c-r"
    $FZF_DEFAULT_COMMAND="fd --type file --color=always --follow --hidden --exclude .git --exclude .hg --exclude node_modules"
    $FZF_DEFAULT_OPTS="--color 'bg:0,bg+:2,fg:8,fg+:15,hl:10,hl+:11,prompt:11,info:3,marker:11,pointer:11,spinner:1' --ansi --height 10"
    $FZF_COMPLETION_TRIGGER=','

    # if os.environ.get('DISPLAY', False):
    #    aliases['lf'] = [path(${'HOME'}, '.config/lf/lfrun')]

    def x11docker(args, stdin=None, stdout=None, stderr=None):
        xhost +
        docker run -e DISPLAY=host.docker.internal:0 @(args)
        xhost -
        return 0

    aliases['x11docker'] = x11docker
    del x11docker
# }}}

xontrib load -s @(_xontribs)
# xontribs_load(_xontribs)

# Abbreviations {{{
if 'abbrevs' not in locals():
    abbrevs = {}
for alias in aliases:
    if alias.startswith('dk') and (t:=type(aliases[alias])) in (list, str):
        if t is list:
            abbrevs[alias] = ' '.join(aliases[alias])
        if t is str:
            abbrevs[alias] = aliases[alias]
# }}}

if 'START_DIR' in __xonsh__.env:
    cd @(__xonsh__.env.get('START_DIR', ''))

execx($(starship init xonsh))
