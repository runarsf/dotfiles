local status_ok, ayu = pcall(require, "ayu")
if not status_ok then
  return
end

-- TODO Load from colorscheme.lua only if colorscheme loaded
local setup = {
  mirage = false, -- Set to `true` to use `mirage` variant instead of `dark` for dark background.
  overrides = {}, -- A dictionary of group names, each associated with a dictionary of parameters (`bg`, `fg`, `sp` and `style`) and colors in hex.
}

ayu.setup(setup)
