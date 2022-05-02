local M = {}

M.scheme = function(opts)
  setmetatable(opts, {__index={scheme="default", config=opts[1], background="dark", source_conf="post"}})
  local scheme, config, background, source_conf =
    opts[1] or opts.scheme,
    opts[2] or opts.config,
    opts[3] or opts.background,
    opts[4] or opts.source_conf

  local source = function(conf)
    if conf then
      local ok, loaded_config = pcall(require, "config." .. conf)
      if ok and type(loaded_config) == "function" then
        loaded_config()
      end
    end
  end

  if config and (source_conf == "pre" or source_conf == "both") then
    source(config)
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

  if config and (source_conf == "post" or source_conf == "both") then
    source(config)
  end
end

return M
