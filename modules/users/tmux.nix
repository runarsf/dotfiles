{ config, pkgs, ... }:

{
  programs.tmux = {
    enable = true;
    plugins = with pkgs.tmuxPlugins; [ catppuccin extrakto resurrect mode-indicator yank sensible ];
    clock24 = true;
    # FIXME Plugins don't get properly configured because this is added to the end of the config file
    extraConfig = ''
      set -g default-shell $SHELL

      set -g display-time 2000

      set -g allow-passthrough on

      # Start window and pane indexing at 1, not 0
      set -g base-index 1
      set -gw pane-base-index 1
      set -g renumber-windows on

      set -g pane-border-indicators both
      set -gw pane-border-lines heavy

      set -gw monitor-activity on
      set -g visual-activity on

      set -gw mouse off

      set -gw mode-keys vi
      # set -gw xterm-keys on
      # }}}

      # set -g pane-border-format "#{pane_index}-#{pane_current_command}"

      # Binds
      unbind -a

      # Does not work correctly when plugins are sourced with source-file
      # bind -n C-Up \
      #   set -gq status-bg colour25 \; \
      #   unbind -a \; \
      #   unbind -na \; \
      #   set -gu prefix C-a \; \
      #   bind -n C-Down 'source-file ${config.home.homeDirectory}/.config/tmux/tmux.conf'

      set -g prefix C-a
      bind C-a send-prefix

      bind r source-file ${config.home.homeDirectory}/.config/tmux/tmux.conf \; display 'Reloaded tmux config'

      bind 0 select-window -t :0
      bind 1 select-window -t :1
      bind 2 select-window -t :2
      bind 3 select-window -t :3
      bind 4 select-window -t :4
      bind 5 select-window -t :5
      bind 6 select-window -t :6
      bind 7 select-window -t :7
      bind 8 select-window -t :8
      bind 9 select-window -t :9

      bind ':' command-prompt
      bind '#' list-buffers
      bind '?' list-keys
      bind '!' break-pane
      bind f command-prompt "find-window '%%'"
      bind x confirm-before -p "kill-pane #P? (y/n)" kill-pane

      bind Space next-layout

      bind d detach-client
      bind D choose-client
      bind L switch-client -l

      bind n new-window
      bind '|' split-window -h -c '#{pane_current_path}'
      bind '-' split-window -v -c '#{pane_current_path}'
      bind o rotate-window
      # bind z resize-pane -Z
      bind j command-prompt -p "join pane from:"  "join-pane -s '%%'"
      bind s command-prompt -p "send pane to:"  "join-pane -t '%%'"
      bind m set mouse \; display 'Toggled mouse mode'

      bind Enter if-shell "[ $(($(tmux display -p '8*#{pane_width}-20*#{pane_height}'))) -lt 0 ]" "splitw -v -c '#{pane_current_path}'" "splitw -h -c '#{pane_current_path}' "
      bind Space run-shell 'bash -c "tmux swap-pane -t \$(tmux list-panes -F \"##P 8*##{pane_width}*20*##{pane_height}\" | while read l; do t=( \$l );echo \"\$\{t[0]\} \$((\$\{t[1]\}))\";done | sort -k 2 -r -g | cut -d\" \" -f1 | head -n 1)"'

      bind-key p command-prompt -p 'Save history to file:' -I '~/Documents/#W.txt' 'capture-pane -S -32768 ; save-buffer %1 ; delete-buffer'

      # Switching panes
      bind -n S-Left select-pane -L
      bind -n S-Down select-pane -D
      bind -n S-Up select-pane -U
      bind -n S-Right select-pane -R

      # Resizing panes
      bind -nr M-S-Left resize-pane -L 5
      bind -nr M-S-Down resize-pane -D 5
      bind -nr M-S-Up resize-pane -U 5
      bind -nr M-S-Right resize-pane -R 5

      bind -n C-M-Left previous-window
      bind -n C-M-Right next-window

      bind -T copt-mode-vi v send-keys -X begin-selection
      bind -T copy-mode-vi C-v send-keys -X rectangle-toggle
      bind -T copy-mode-vi y send-keys -X copy-selection-and-cancel

      # Plugins
      set -g @extrakto_key x
      set -g @catppuccin_flavour 'ayu'
      set -g @catppuccin_status_left_separator "█"
      set -g @catppuccin_status_modules "application session user host date_time"
    '';
  };
}
