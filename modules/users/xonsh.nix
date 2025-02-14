{
  config,
  pkgs,
  outputs,
  ...
}:
# TODO Cachix for xonsh-xontribs https://github.com/drmikecrowe/nur-packages?tab=readme-ov-file#cachix-public-keys
# https://github.com/oh-my-xonsh/xontrib-default-command
# https://pypi.org/project/xontrib-brace-expansion/
# https://github.com/anki-code/xontrib-pipeliner
# Redirect output of python command to file
#   >>> echo @(__import__('inspect').getsource($PROMPT_FIELDS['gitstatus'])) >> FILE
# Inspect function
#   >>> __import__('inspect').getsource($PROMPT_FIELDS['gitstatus'])
outputs.lib.mkModule config "xonsh" {
  nixos.programs.xonsh = with pkgs.nur.repos.xonsh-xontribs; {
    enable = true;
    package = xonsh-wrapped.override {
      extraPackages = ps: [
        xontrib-term-integrations
        xontrib-bashisms
        xontrib-dot-dot
        xontrib-zoxide
        backtrace
        xontrib-sh
        xontrib-abbrevs
        xontrib-whole-word-jumping
        xontrib-prompt-starship
      ];
    };
    config = ''
      from xonsh.xontribs import xontribs_load as _xontribs_load

      _xontribs = [
          "coreutils",
          "bashisms",
          "abbrevs",
          "prompt_starship",
          "sh",
          "term_integration",
          "whole_word_jumping",
          "zoxide",
      ]

      # Environment variables - https://xon.sh/envvars.html
      $AUTO_CONTINUE = True
      $COMPLETIONS_CONFIRM = True
      $HISTCONTROL = {'ignoredups', 'ignorespace'}
      $MULTILINE_PROMPT = ' '
      $AUTO_CD = True
      $FOREIGN_ALIASES_SUPPRESS_SKIP_MESSAGE = True
      $XONSH_SHOW_TRACEBACK = True
      $ENABLE_ASYNC_PROMPT = True
      $VC_BRANCH_TIMEOUT = 0.2
      $AUTO_PUSHD = True
      $CASE_SENSITIVE_COMPLETIONS = False
      $DYNAMIC_CWD_ELISION_CHAR = '…'
      $AUTO_SUGGEST_IN_COMPLETIONS = True
      $XONSH_AUTOPAIR = True

      # JSON support
      try:
          import __builtin__
      except ImportError:
          import builtins as __builtin__
      __builtin__.true = True
      __builtin__.false = False
      __builtin__.null = None

      xontrib load -s @(_xontribs)

      if 'abbrevs' not in locals():
          abbrevs = {}
      for alias in aliases:
          if alias.startswith('dk') and (t:=type(aliases[alias])) in (list, str):
              if t is list:
                  abbrevs[alias] = ' '.join(aliases[alias])
              if t is str:
                  abbrevs[alias] = aliases[alias]
    '';
    /*
    config = ''
        from os.path import (realpath as _realpath, join as _join)
        from shutil import which as _which
        from xonsh.platform import ON_LINUX as _ON_LINUX

        # Helper function to join paths and get full path.
        path = lambda *arg: _realpath(_join(*arg))

        # Aliases {{{
        # aliases['ls'] = ['ls', '-l', '-A', '-F', '-h']
        # if $(ls --version 2>/dev/null).strip():
        #     aliases['ls'].append('--color=auto')
        # if _which('exa') is not None:
        #     aliases['ls'] = ['exa', '--group-directories-first', '--git', '--long', '--all']
        aliases['ls'] = ['${pkgs.eza}/bin/eza', '-l', '-F', '-g', '-a', '--group-directories-first', '--no-time', '--git']

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
    '';
    */
  };
}
