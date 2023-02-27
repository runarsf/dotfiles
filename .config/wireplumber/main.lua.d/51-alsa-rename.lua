rules = {
  {
    matches = { { {
      "node.name", "equals", "alsa_output.usb-Astro_Gaming_Astro_MixAmp_Pro-00.pro-output-0"
    }, }, },
    apply_properties = {
      ["node.description"] = "Astro MixAmp Pro Game",
    },
  },
  {
    matches = { { {
      "node.name", "equals", "alsa_output.usb-Astro_Gaming_Astro_MixAmp_Pro-00.pro-output-1"
    }, }, },
    apply_properties = {
      ["node.description"] = "Astro MixAmp Pro Voice",
    },
  },
}

for k, v in pairs(rules) do
  table.insert(alsa_monitor.rules, v)
end
