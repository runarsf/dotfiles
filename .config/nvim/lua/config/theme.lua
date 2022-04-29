local Colors = {}
Colors.scheme = "default"
Colors.background = "dark"

Colors.scheme = function(scheme, scheme_config, background)
  scheme = scheme or Colors.scheme
  background = background or Colors.background
  scheme_config = scheme_config or scheme

  local ok, _ = pcall(vim.cmd, "colorscheme " .. scheme)
  if ok then
    vim.cmd("set background=" .. background)
  else
    vim.cmd [[
      colorscheme default
      set background=dark
    ]]
  end

  local ok, loaded_config = pcall(require, "config." .. scheme_config)
  if ok and type(loaded_config) == "function" then
    loaded_config()
  end
end

return Colors