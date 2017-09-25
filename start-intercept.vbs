'
' Runs intercept.exe (which reads keyremap.ini) to remap the keys so AHK can work with them.
'
' This is done using a VB script so that intercept.exe can be run without a command prompt window being left active
'
Dim wsh

Set wsh = CreateObject("Wscript.Shell")

' Must set current directory, as intercept.exe reads its keyremap.ini file from the current directory
path = CreateObject("Scripting.FileSystemObject").GetParentFolderName(Wscript.ScriptFullName)
wsh.CurrentDirectory = path

' /apply option means start remapping using the keyremap.ini file in the current directory
wsh.Run "intercept /apply", 0, True


