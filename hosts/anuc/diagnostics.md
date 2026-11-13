## 2026-11-13

```sh
$ sudo smartctl -A /dev/sda | grep -E 'CRC_Error|Added_Bad|POR_Recovery|Power_Cycle|Power_On'
  9 Power_On_Hours          0x0032   095   095   000    Old_age   Always       -       21262
 12 Power_Cycle_Count       0x0032   099   099   000    Old_age   Always       -       62
199 CRC_Error_Count         0x003e   099   099   000    Old_age   Always       -       187
235 POR_Recovery_Count      0x0012   099   099   000    Old_age   Always       -       46
252 Added_Bad_Flash_Blk_Ct  0x0032   100   100   000    Old_age   Always       -       21
```

Later (like in a month), run `sudo smartctl -A /dev/sda | grep -E 'CRC_Error|Added_Bad'` and compare against the baseline.
CRC was 187. If it hasn't moved, the SATA cable needs to be replaced.
If it's still climbing, move to an M.2 slot, put the system on an NVMe drive and the SATA link won't be an issue anymore.
