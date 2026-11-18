nav = {}

-- Special handling for dir, if any: "group" or "stack", plus a forward flag.
local function scope(window, dir)
  local group = window.group
  local index = group and group.current_index
  local size = (group and group.size) or 0

  if index and dir == "left" and index > 1 then
    return "group", false
  elseif index and dir == "right" and index < size then
    return "group", true
  end

  local workspace = window.workspace

  if workspace and workspace.has_fullscreen then
    if dir == "up" then
      return "stack", false
    elseif dir == "down" then
      return "stack", true
    end
  end
end

local function focus_direction(dir)
  hl.dispatch(hl.dsp.focus({ direction = dir }))
  hl.dispatch(hl.dsp.cursor.move_to_corner({ corner = 2 }))
end

function nav.focus(dir)
  local window = hl.get_active_window()

  -- Focus can still leave an empty workspace.
  if not window then
    focus_direction(dir)
    return
  end

  local what, forward = scope(window, dir)

  if what == "group" then
    hl.dispatch(forward and hl.dsp.group.next() or hl.dsp.group.prev())
  elseif what == "stack" then
    hl.dispatch(hl.dsp.window.cycle_next({ next = forward }))
  else
    focus_direction(dir)
  end
end

function nav.move(dir)
  local window = hl.get_active_window()

  if not window then
    return
  end

  local what, forward = scope(window, dir)

  if what == "group" then
    hl.dispatch(hl.dsp.group.move_window({ forward = forward }))
  elseif what == "stack" then
    hl.dispatch(hl.dsp.window.swap({ next = forward }))
  else
    hl.dispatch(hl.dsp.window.move({ direction = dir }))
  end
end
