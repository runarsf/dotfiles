#!/usr/bin/env python3
import keyboard
import evdev

deviceList = []

devices = [evdev.InputDevice(path) for path in evdev.list_devices()]
for device in devices:
  deviceList.append({
    'path': device.path,
    'name': device.name,
    'phys': device.phys
  })

print('Please select a device:')
for i, device in enumerate(deviceList, start=0):
  print(f' {i}: {device["name"]}')

selectedDevice = int(input('> '))
print(f'Selected device \'{deviceList[selectedDevice]["name"]}\'')