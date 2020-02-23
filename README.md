# dotfiles

order of config calls
```bash
# Attempting to detect run orde with this in the top of the relevant files
echo "$(basename "$0") @ $(date +%T.%N)" >> /tmp/bootorder
```

git submodule update --recursive --init

### Enable bitmap fonts:
```bash
sudo rm /etc/fonts/conf.d/70-no-bitmaps.conf
sudo ln -s /etc/fonts/conf.avail/70-yes-bitmaps.conf /etc/fonts/conf.d/70-yes-bitmaps.conf
```
