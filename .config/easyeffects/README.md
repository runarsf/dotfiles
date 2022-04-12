## Presets

- https://gist.github.com/sebastian-de/648555c1233fdc6688c0a224fc2fca7e
- https://github.com/Rabcor/Heavy-Bass-EE
- https://github.com/jaakkopasanen/AutoEq/blob/master/results/crinacle/gras_43ag-7_harman_over-ear_2018/Beyerdynamic%20DT%20770%2080%20Ohm/Beyerdynamic%20DT%20770%2080%20Ohm%20ParametricEQ.txt
- https://github.com/JackHack96/EasyEffects-Presets

## [Fix broken convolver plugin](https://forum.manjaro.org/t/howto-enhance-your-linux-audio-with-easyeffects-or-pulseeffects-legacy/82497)

`grep "kernel-path" "${1:?}" | sed 's:"k:\n"k:g' | sed 's:",:"\n:g'`
