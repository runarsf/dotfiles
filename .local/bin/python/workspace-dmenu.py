#!/usr/local/bin/python3
import os
import subprocess
import i3ipc

def main():
    i3 = i3ipc.Connection()
    with_no_num_workspaces = list(filter(lambda w: w.num == -1, i3.get_workspaces()))
    menu_items = [w["name"] for w in with_no_num_workspaces]
    with subprocess.Popen(["dmenu", "-i", "-l", "10", "-sb", "red"], stdin=subprocess.PIPE, stdout=subprocess.PIPE) as proc:
        result = proc.communicate("\n".join(menu_items).encode("utf-8"))[0]
        workspace = result.decode("utf-8").strip()
        open_workspace_by_name(workspace, with_no_num_workspaces, i3)

def open_workspace_by_name(value, with_no_num_workspaces, i3):
    selected_workspace = next(filter(lambda w: w.name.lower() == value.lower(), with_no_num_workspaces))
    i3.command('workspace "{}"'.format(selected_workspace.name))

if __name__ == "__main__":
    main()
