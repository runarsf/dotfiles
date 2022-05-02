local M = {}

M.scheme = function(opts)
  setmetatable(opts, {__index={scheme="default", config=opts[1], background="dark", pre_conf=false}})
  local scheme, config, background, pre_conf =
    opts[1] or opts.scheme,
    opts[2] or opts.config,
    opts[3] or opts.background,
    opts[4] or opts.pre_conf

  local source_conf = function(conf)
    if conf then
      local ok, loaded_config = pcall(require, "config." .. conf)
      if ok and type(loaded_config) == "function" then
        loaded_config()
      end
    end
  end

  if config and pre_conf then
    source_conf(config)
  end

  local ok, _ = pcall(vim.cmd, "colorscheme " .. scheme)
  if ok then
    vim.cmd("set background=" .. background)
  else
    vim.cmd [[
      colorscheme default
      set background=dark
    ]]
  end

  if config and not pre_conf then
    source_conf(config)
  end
end

return M
