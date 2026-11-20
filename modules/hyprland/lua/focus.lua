-- input.focus_on_close = 2 refocuses the previously focused window, which may
-- sit on another monitor. When the last window on a monitor closes, stay there.

local function other_windows_visible(monitor, closing)
  for _, window in ipairs(hl.get_windows({ monitor = monitor.name })) do
    local workspace = window.workspace

    if window.address ~= closing.address and workspace and workspace.visible then
      return true
    end
  end

  return false
end

-- The close event may arrive after the refocus, so the window's own active flag
-- can already be gone; the cursor still points at the monitor (no_warps).
local function was_focused(window, monitor)
  if window.active then
    return true
  end

  local at_cursor = hl.get_monitor_at_cursor()

  return at_cursor ~= nil and at_cursor.id == monitor.id
end

hl.on("window.close", function(window)
  local monitor = window.monitor

  if not monitor or not was_focused(window, monitor) then
    return
  end

  if other_windows_visible(monitor, window) then
    return
  end

  local id, name = monitor.id, monitor.name

  -- Hyprland applies focus_on_close after this event, so check back later.
  hl.timer(function()
    local active = hl.get_active_monitor()

    if active and active.id ~= id then
      hl.dispatch(hl.dsp.focus({ monitor = name }))
    end
  end, { timeout = 20, type = "oneshot" })
end)
