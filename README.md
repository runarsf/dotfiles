# dotfiles

order of config calls
```bash
# Attempting to detect run orde with this in the top of the relevant files
echo "$(basename "$0") @ $(date +%T.%N)" >> /home/drift/bootorder
```

git submodule update --recursive --init
