#!/usr/bin/env python3
import os
import subprocess

# List the IDs (or names) of the matching monitors.
# -> CompletedProcess(args=['bspc', 'query', '-M'], returncode=0, stdout=b'0x0040000E\n0x00400010\n0x00400012\n')
monitors_output = subprocess.run(['bspc', 'query', '--monitors', '--names'], stdout=subprocess.PIPE)

# Parse the monitor list command into an array of monitor IDs
# -> ['0x0040000E', '0x00400010', '0x00400012']
monitors = monitors_output.stdout.decode('utf-8').strip().split('\n')

# Move the primary monitor to the front
primary_output = subprocess.run('xrandr | grep "connected primary" | cut -d" " -f1', stdout=subprocess.PIPE, shell=True)
primary = primary_output.stdout.decode('utf-8').strip()
monitors.remove(primary)
monitors.insert(0, primary)

# Parse an a string of strings to an awway of available workspaces
# -> ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0']
workspaces = os.environ.get('BSPWM_WORKSPACES', '1 2 3 4 5 6 7 8 9 0').split(' ')

assigned = {}

# Populate assigned{monitor['workspace']}
i = 0
while not i >= len(workspaces):
    for monitor in monitors:
        try:
            assigned[monitor] += workspaces[i]
        except KeyError:
            assigned[monitor] = workspaces[i]

        i += 1
        if i >= len(workspaces):
            break

print(assigned)
for monitor in monitors:
    command = ['bspc', 'monitor', monitor, '--reset-desktops']
    command.extend(list(assigned[monitor]))
    print(command)
    subprocess.call(command)
