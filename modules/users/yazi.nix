{
  config,
  pkgs,
  outputs,
  ...
}:
outputs.lib.mkModule config "yazi" {
  home.packages = with pkgs; [exiftool];
  # NOTE This only works for posix compliant shells, not nushell
  # home.shellAliases."fm" = ''${config.programs.yazi.package}/bin/yazi --cwd-file /tmp/yazi.dir "$@" && cd "$(\cat /tmp/yazi.dir)"'';

  xdg.configFile = {
    "yazi/init.lua".text = ''
      function Status:name()
        local h = self._tab.current.hovered
        if not h then
          return ui.Line {}
        end

        local linked = ""
        if h.link_to ~= nil then
          linked = " -> " .. tostring(h.link_to)
        end
        return ui.Line(" " .. h.name .. linked)
      end

      Status:children_add(function()
        local h = cx.active.current.hovered
        if h == nil or ya.target_family() ~= "unix" then
          return ui.Line {}
        end

        return ui.Line {
          ui.Span(ya.user_name(h.cha.uid) or tostring(h.cha.uid)):fg("magenta"),
          ui.Span(":"),
          ui.Span(ya.group_name(h.cha.gid) or tostring(h.cha.gid)):fg("magenta"),
          ui.Span(" "),
        }
      end, 500, Status.RIGHT)

      Header:children_add(function()
        if ya.target_family() ~= "unix" then
          return ui.Line {}
        end
        return ui.Span(ya.user_name() .. "@" .. ya.host_name() .. ":"):fg("blue")
      end, 500, Header.LEFT)
    '';
    "yazi/plugins/smart-enter.yazi/init.lua".text = ''
      return {
        entry = function()
          local h = cx.active.current.hovered
          ya.manager_emit(h and h.cha.is_dir and "enter" or "open", { hovered = true })
        end,
      }
    '';
    "yazi/plugins/smart-paste.yazi/init.lua".text = ''
      return {
        entry = function()
          local h = cx.active.current.hovered
          if h and h.cha.is_dir then
            ya.manager_emit("enter", {})
            ya.manager_emit("paste", {})
            ya.manager_emit("leave", {})
          else
            ya.manager_emit("paste", {})
          end
        end,
      }
    '';
  };

  programs.yazi = {
    enable = true;
    settings = {
      log.enabled = false;
      manager = {
        show_hidden = true;
        show_symlink = true;
        scrolloff = 7;
        ratio = [
          1
          3
          4
        ];
        sort_translit = true;
      };
      opener = {
        edit = [
          {
            run = ''''${EDITOR:-vim} "$@"'';
            block = true;
            for = "unix";
          }
        ];
      };
    };
    keymap = {
      manager = {
        prepend_keymap = [
          {
            on = "<Esc>";
            run = "close";
            desc = "Cancel input";
          }
          {
            on = "e";
            # "shell '\${EDITOR} \"$@\"' --confirm --block";
            run = [
              "open"
              "quit"
            ];
            desc = "Open for editing";
          }
          {
            on = "<Enter>";
            run = "plugin --sync smart-enter";
            desc = "Enter the child directory, or open the file";
          }
          {
            on = "p";
            run = "plugin --sync smart-paste";
            desc = "Paste into the hovered directory or CWD";
          }
          {
            on = [
              "d"
              "r"
            ];
            run = "shell '${pkgs.xdragon}/bin/dragon -x -i -T \"$1\"' --confirm";
            desc = "Drag-and-drop the hovered file";
          }
          {
            on = [
              "d"
              "d"
            ];
            run = "delete";
          }
          {
            on = "Y";
            run = [
              "shell 'for path in \"$@\"; do echo \"file://$path\"; done | wl-copy -t text/uri-list' --confirm"
              "yank"
            ];
            desc = "Yank and copy";
          }
        ];
      };
    };
  };
}
