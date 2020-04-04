#!/usr/bin/env python3
import os
import subprocess

output = subprocess.run(['bspc', 'query', '-M'], stdout=subprocess.PIPE)
monitors = output.stdout.decode('utf-8').strip().split('\n')

workspaces = os.environ.get('BSPWM_WORKSPACES', '0 1 2 3 4 5 6 7 8 9').split(' ')

assigned = {}

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

for monitor in monitors:
    command = ['bspc', 'monitor', monitor, '-d']
    command.extend(list(assigned[monitor]))
    print(command)
    subprocess.call(command)
