# vim: set syntax=python foldmethod=marker:
import os
from shutil import which

# Stuff:
# - Redirect output of python command to file
#   >>> echo @(__import__('inspect').getsource($PROMPT_FIELDS['gitstatus'])) >> FILE
# - Inspec function
#   >>> __import__('inspect').getsource($PROMPT_FIELDS['gitstatus'])

# Helper function to join paths and get full path.
path = lambda *arg: os.path.realpath(os.path.join(*arg))

# Environment variables - https://xon.sh/envvars.html {{{

# The SQLite history backend saves command immediately
# unlike JSON backend that save the commands at the end of the session.
$XONSH_HISTORY_BACKEND = 'json'

# What commands are saved to the history list. By default all commands are saved.
# * The option 'ignoredups' will not save the command if it matches the previous command.
# * The option 'erasedups' will remove all previous commands that matches and updates the command frequency.
#   The minus of 'erasedups' is that the history of every session becomes unrepeatable
#   because it will have a lack of the command you repeat in another session.
# Docs: https://xonsh.github.io/envvars.html#histcontrol
$HISTCONTROL = 'ignoredups'

# Remove front dot in multiline input to make the code copy-pastable.
$MULTILINE_PROMPT = ' '

# Avoid typing cd just directory path.
# Docs: https://xonsh.github.io/envvars.html#auto-cd
$AUTO_CD = True

# Sets the color style for xonsh colors.
# Run `xonfig styles` to see the available styles.
$XONSH_COLOR_STYLE = 'default'

# Suppress the message that informs the user when a foreign alias has been skipped
# because it already exists in xonsh.
$FOREIGN_ALIASES_SUPPRESS_SKIP_MESSAGE = True

# Show a traceback when exceptions occur in the shell.
$XONSH_SHOW_TRACEBACK = True

# Configure if and how Python completions are displayed by the `prompt_toolkit` shell.
# This option does not affect Bash completions, auto-suggestions, etc.
$COMPLETIONS_DISPLAY = 'multi'

# When enabled the prompt is rendered using threads.
# Should be True, but eeeeeh
$ENABLE_ASYNC_PROMPT = False

# The timeout (in seconds) for version control branch computations.
# This is a timeout per subprocess call,
# so the total time to compute will be larger than this in many cases.
$VC_BRANCH_TIMEOUT = .2

# Automatically push directories onto the directory stack.
$AUTO_PUSHD = True

# Sets whether completions should be case sensitive or case insensitive.
$CASE_SENSITIVE_COMPLETIONS = False

# The string used to show a shortened directory in a shortened cwd.
$DYNAMIC_CWD_ELISION_CHAR = '…'

# nable automatic command suggestions based on history, like in the fish shell.
$AUTO_SUGGEST = True

# Places the auto-suggest result as the first option in the completions.
# This enables you to tab complete the auto-suggestion.
$AUTO_SUGGEST_IN_COMPLETIONS = True

# Press <Enter> to confirm completion in tab-completions menu instead of running command.
# This only affects the prompt-toolkit shell.
$COMPLETIONS_CONFIRM = True

# Auto-insert matching parentheses, brackets, and quotes.
# Only available under the prompt-toolkit shell.
$XONSH_AUTOPAIR = True

# Value and units tuple that sets the size of history after garbage collection. Canonical units are:
#   `commands` for the number of past commands executed,
#   `files` for the number of history files to keep,
#   `s` for the number of seconds in the past that are allowed, and
#   `b` for the number of bytes that history may consume.
# Common abbreviations, such as '6 months' or '1 GB' are also allowed.
$XONSH_HISTORY_SIZE = (1073741824, 'commands')

# Non-xonsh-related environment variables
$NPM_PACKAGES = path(${'HOME'}, '.npm')
$PYENV_ROOT = path(${'HOME'}, '.pyenv')
$GOPATH = path(${'HOME'}, 'go/bin')

$EDITOR = 'nvim'
# }}}

# Prompt customization - https://xon.sh/tutorial.html#customizing-the-prompt {{{
# Seems to only work with `$ENABLE_ASYNC_PROMPT = False`, which is dumb.
# https://github.com/anki-code/xontrib-prompt-bar

def _prompt_ret_color(): # {{{
    if __xonsh__.history.rtns and __xonsh__.history.rtns[-1] != 0:
        return '{#cc6666}' # red
    else:
        return '{#ffffff}' # '{#f0c674}'

$PROMPT_FIELDS['ret_color'] = _prompt_ret_color
del _prompt_ret_color
# }}}

# def _prompt_git_prefix(): # {{{
#     import xonsh.tools as xt
# 
#     prefix = $(git rev-parse --show-prefix 2>/dev/null).strip()
#     sep = xt.get_sep()
# 
#     if len(prefix) == 0:
#         return prefix
#     return sep + prefix if prefix[0] != sep else prefix

# $PROMPT_FIELDS['git_prefix'] = _prompt_git_prefix
# del _prompt_git_prefix
# }}}

def _prompt_git_status(): # {{{
    from re import sub as rs

    _status = $PROMPT_FIELDS['gitstatus']() or ''
    # Can you replace $PROMPT_FIELDS['branch_color'] instead?
    _status = rs(r'^\{.+?\}', '{RED}', _status, count=1)
    _status = rs(r'\|', '', _status, count=1)

    return '{INTENSE_BLACK}['+_status+'{INTENSE_BLACK}]' if _status else None

$PROMPT_FIELDS['git_status'] = _prompt_git_status
del _prompt_git_status
# }}}

def _prompt_git_cwd(): # {{{
    import xonsh.tools as xt
    from xonsh.prompt.cwd import _replace_home

    toplevel = $(git rev-parse --show-toplevel 2>/dev/null).strip()
    pwd = toplevel if toplevel else $PWD

    sep = xt.get_sep()
    pwd = _replace_home(pwd).split(sep)
    l = len(pwd)
    leader = sep if l > 0 and len(pwd[0]) == 0 else ""
    base = [i[0]
            if ix != l - 1 and i[0] != '.' else i[0:2]
            if ix != l - 1 else i for ix, i in enumerate(pwd) if len(i) > 0]
    return leader + sep.join(base)

$PROMPT_FIELDS['git_cwd'] = _prompt_git_cwd
del _prompt_git_cwd
# }}}

# PROMPT_FIELDS {{{
$PROMPT_FIELDS['prompt_end'] = '@' if $PROMPT_FIELDS['prompt_end'] == '$' else '#' # '🐚'
$PROMPT_FIELDS['short_username'] = ${'USER'}[:1]
$PROMPT_FIELDS['short_hostname'] = ${'HOSTNAME'}[:1]
# }}}

$PROMPT = '{0}{1}'.format('{RESET}'.join([
    '{WHITE}{env_name}',
    '{INTENSE_BLACK}[',
    '{#D99824}{short_username}',
    '{#989718}@',
    '{#42858B}{short_hostname}',
    ' {#B26287}{git_cwd}',
    '{INTENSE_BLACK}]',
    '{git_status: {}}',
    '{ret_color}{prompt_end} ',
]), '{RESET}')
# }}}

# Aliases {{{
aliases['-'] = ['cd', '-']
if which('exa') is not None:
    aliases['ls'] = ['exa', '--group-directories-first', '--git', '--long', '--all']
else:
    aliases['ls'] = ['ls', '-l', '-A', '-F', '-h']
    if $(ls --version 2>/dev/null).strip():
        aliases['ls'].append('--color=auto')
aliases['grep'] = ['grep', '--color']
aliases['vim'] = ${'EDITOR'}

aliases['dk']    = ['docker']
aliases['dkc']   = ['docker-compose']
aliases['dkcL']  = ['docker-compose', 'logs', '-F']
aliases['dkcb']  = ['docker-compose', 'build']
aliases['dkcB']  = ['docker-compose', 'build', '--no-cache']
aliases['dkcd']  = ['docker-compose', 'down']
aliases['dkcU']  = ['docker-compose', 'up', '-d']
aliases['dkcUf'] = ['docker-compose', 'up', '-d', '--force-recreate', '--remove-orphans']
# }}}

# Base xontribs - https://xon.sh/xontribs.html  |  Github topic with thumbnails - https://github.com/topics/xontrib {{{
_xontribs = [
    'mpl',                # Matplotlib hooks for xonsh.
    'prompt_ret_code',    # Adds return code info to the prompt.
    'abbrevs',            # Command abbreviations (expandable aliases).
    'readable-traceback', # Make traceback easier to read.
    'whole_word_jumping', # Jumping across whole words (non-whitespace) with Ctrl+Left/Right and Alt+Left/Right.
    'argcomplete',        # Argcomplete support to tab completion of python and xonsh scripts in xonsh.
    'output_search',      # Get identifiers from previous command and use for next command -> <identifier><Alt-f>.
    # 'macro_lib',          # Library of the useful macros for the xonsh shell.
]
# }}}

# $BASH_COMPLETIONS.insert(0, '/usr/local/share/bash-completion/bash_completion/') # '/usr/local/bin/bash-completion/'

# Add directories to $PATH {{{
for pathdir in reversed([
        path(${'HOME'}, '.local/bin'),
        path(${'HOME'}, '.cargo/bin'),
        path(${'HOME'}, 'bin'),
        path(${'GOPATH'}[0], 'bin'),
        path(${'NPM_PACKAGES'}, 'bin'),
        path(${'PYENV_ROOT'}, 'bin'),
        '/opt/X11/bin',
        '/usr/local/bin',
        '/usr/local/sbin',
    ]):
    if pathdir not in ${'PATH'} and os.path.isdir(pathdir):
        ${'PATH'}.insert(0, pathdir)
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
from xonsh.platform import ON_LINUX  # ON_DARWIN, ON_WINDOWS, ON_WSL, ON_CYGWIN, ON_MSYS, ON_POSIX, ON_FREEBSD, ON_DRAGONFLY, ON_NETBSD, ON_OPENBSD


if ON_LINUX:

    # if which('zsh') is not None:
    #     source-foreign zsh --overwrite-aliases --interactive True --sourcer source 'echo Loading foreign shell zsh.'

    _xontribs = list(set([
        'coreutils',          # Additional core utilities that are implemented in xonsh.
        'apt_tabcomplete',    # Adds tabcomplete functionality to apt-get/apt-cache.
        'fzf-widgets',        # Adds some fzf widgets.
        'zoxide',             # - curl -sS https://webinstall.dev/zoxide | bash
        'sh',                 # Paste and run commands from bash, zsh, fish in xonsh.
        # 'docker_tabcomplete', # Adds tabcomplete functionality to docker. Broken for docker-py >= 2.0.0.
        # 'gitinfo',            # Displays git information on entering a repository folder.
        # 'pipeliner',          # Let your pipe lines flow thru the Python code in xonsh.
        # 'distributed',        # The distributed parallel computing library hooks for xonsh.
        # 'bashisms',           # Enables additional Bash-like syntax while at the command prompt.
        # 'prompt_starship'     # Starship cross-shell prompt in xonsh shell.
    ] + _xontribs))

    def _git_bare(git_dir, work_tree, *args):
        if not args:
            args = ('status')
        git --git-dir=@(git_dir) --work-tree=@(work_tree) @(args)

    $DOTBARE_DIR = path(${'HOME'}, '.config/dotfiles/.git')
    $DOTBARE_TREE = path(${'HOME'})

    aliases['dotfiles']  = lambda args: _git_bare(${'DOTBARE_DIR'}, ${'DOTBARE_TREE'}, *args)
    aliases['dots']      = ['dotfiles']
    aliases['dirtydots'] = ['dirtygit', '--git-dir', ${'DOTBARE_DIR'}, '--work-tree', ${'DOTBARE_TREE'}, '--git-add', '-u']
    aliases['dotted']    = ['dotfiles', 'ls-files', '--error-unmatch']
    aliases['dkx']       = lambda args: $[docker exec -it @(args) sh]
    aliases['gitted']    = ['git', 'ls-files', '--error-unmatch']
    aliases['c']         = ['xclip', '-selection', 'clipboard']
    aliases['t']         = ['todo.sh', '-a', '-c']

    $fzf_history_binding = "c-r"
    $FZF_DEFAULT_COMMAND="fd --type file --color=always --follow --hidden --exclude .git --exclude .hg --exclude node_modules"
    $FZF_DEFAULT_OPTS="--color 'bg:0,bg+:2,fg:8,fg+:15,hl:10,hl+:11,prompt:11,info:3,marker:11,pointer:11,spinner:1' --ansi --height 10"
    $FZF_COMPLETION_TRIGGER=','

    if os.environ.get('DISPLAY', False):
        aliases['lf'] = [path(${'HOME'}, '.config/lf/lfrun')]

    # Add ssh-key to keychain.
    # if (os.path.isfile('/usr/bin/security') and
    #     !(ssh-add -lq a> /dev/null).returncode == 1):
    #     # -K is only available on macOS.
    #     ssh-add

    # def ssh(args, stdin=None):
    #     $TERM = 'xterm-256color'
    #     ssh @(args) < @(stdin)

    # aliases['ssh'] = ssh
    # del ssh

    def x11docker(args, stdin=None, stdout=None, stderr=None):
        xhost +
        docker run -e DISPLAY=host.docker.internal:0 @(args)
        xhost -
        return 0

    aliases['x11docker'] = x11docker
    del x11docker

    # GUI sudo-askpass for non SSH sessions
    # if 'SSH_CLIENT' not in ${...}:
    #   aliases['sudo'] = 'sudo -A'
    #   $SUDO_ASKPASS = path($HOME, '.local/bin/rofi-askpass')
    #   $SSH_ASKPASS = path($HOME, '.local/bin/rofi-askpass')
# }}}

# Load xontribs
if _xontribs:
    xontrib load @(_xontribs)

# These have to be set after the 'abbrevs' xontrib is loaded {{{
if ON_LINUX:
    abbrevs['dk']     = ' '.join(aliases['dk'])
    abbrevs['dkc']    = ' '.join(aliases['dkc'])
    abbrevs['dkcL']   = ' '.join(aliases['dkcL'])
    abbrevs['dkcb']   = ' '.join(aliases['dkcb'])
    abbrevs['dkcB']   = ' '.join(aliases['dkcB'])
    abbrevs['dkcd']   = ' '.join(aliases['dkcd'])
    abbrevs['dkcU']   = ' '.join(aliases['dkcU'])
    abbrevs['dkcUl']  = ' '.join(aliases['dkcU'])  + ' && ' + ' '.join(aliases['dkcL'])
    abbrevs['dkcUf']  = ' '.join(aliases['dkcUf'])
    abbrevs['dkcUfl'] = ' '.join(aliases['dkcUf']) + ' && ' + ' '.join(aliases['dkcL'])
# }}}
