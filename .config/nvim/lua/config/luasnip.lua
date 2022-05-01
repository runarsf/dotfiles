local cfg = require("util")
local ok, luasnip = pcall(require, "luasnip")
if not ok then return end

luasnip.config.set_config({
  history = false,
  -- Update more often, :h events for more info.
  updateevents = "TextChanged,TextChangedI",
})

require("luasnip/loaders/from_vscode").load()

--- <tab> to jump to next snippet's placeholder
local function on_tab()
  return luasnip.jump(1) and "" or util.t("<Tab>")
end

--- <s-tab> to jump to next snippet's placeholder
local function on_s_tab()
  return luasnip.jump(-1) and "" or util.t("<S-Tab>")
end

cfg.map("i", "<Tab>", on_tab, { expr=true })
cfg.map("s", "<Tab>", on_tab, { expr=true })
cfg.map("i", "<S-Tab>", on_s_tab, { expr=true })
cfg.map("s", "<S-Tab>", on_s_tab, { expr=true })
