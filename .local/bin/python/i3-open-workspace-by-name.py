#!/usr/local/bin/python3
import i3ipc
import sys

def main():
    if len(sys.argv) != 2:
        print("usage error, python script.py workspace-name")
        sys.exit(1)

    workspace_name = sys.argv[1:][0]
    open_workspace(workspace_name)

def open_workspace(name):
    i3 = i3ipc.Connection()
    workspaces = i3.get_workspaces()
    selected_workspace = next(filter(lambda w: w.name.lower() == name.lower(), workspaces))
    i3.command('workspace "{}"'.format(selected_workspace.name))

if __name__ == "__main__":
    main()
