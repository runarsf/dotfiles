#!/usr/bin/env bash
set -euo pipefail

#=============================================================================
# Niri Sidebar Manager
#=============================================================================
# This script manages floating windows in the Niri window manager,
# organizing them into a toggleable sidebar.
#=============================================================================

### --- DIMENSION SETTINGS --- ###
readonly WIN_W=550               # Floating window width
readonly BASE_SLOTS=4            # Default number of slots in the sidebar
readonly GAP_BETWEEN=15          # Gap between windows
readonly BOTTOM_MARGIN=50        # Bottom screen margin
readonly TOP_MARGIN=30           # Top screen margin
readonly SIDE_MARGIN=20          # Side screen margin
readonly VISIBLE_SLIVER=70       # Pixels visible when sidebar is hidden

### --- STATE FILES --- ###
# Get current workspace ID
WORKSPACE_ID=$(niri msg --json workspaces | jq -r '.[] | select(.is_focused).id')

# File paths for state tracking
readonly FLAG_FILE="/tmp/niri.toggle.${WORKSPACE_ID}.floating"
readonly STATE_FILE="/tmp/niri.toggle.${WORKSPACE_ID}.state"
readonly ORDER_STATE_FILE="/tmp/niri.toggle.${WORKSPACE_ID}.order"
readonly HIDE_STATE_FILE="/tmp/niri.toggle.${WORKSPACE_ID}.hide_state"

### --- SCREEN INFO --- ###
readonly SCREEN_W=$(niri msg --json outputs | jq -r 'to_entries[0].value.logical.width')
readonly SCREEN_H=$(niri msg --json outputs | jq -r 'to_entries[0].value.logical.height')

#=============================================================================
# FUNCTION: get_focused_window
# Description: Returns JSON info of the currently focused window
#=============================================================================
get_focused_window() {
  niri msg --json windows | jq -r '.[] | select(.is_focused)'
}

#=============================================================================
# FUNCTION: ensure_files_exist
# Description: Creates necessary state files if they don't exist
#=============================================================================
ensure_files_exist() {
  touch "$FLAG_FILE" "$STATE_FILE" "$HIDE_STATE_FILE" "$ORDER_STATE_FILE"

  if [ ! -s "$HIDE_STATE_FILE" ]; then
      echo "visible" > "$HIDE_STATE_FILE"
  fi

  if [ ! -s "$ORDER_STATE_FILE" ]; then
      echo "bottom-up" > "$ORDER_STATE_FILE"
  fi
}


#=============================================================================
# FUNCTION: get_floating_windows
# Description: Gets a list of floating window IDs in the current workspace
#=============================================================================
get_floating_windows() {
  niri msg --json windows | \
      jq -r ".[] | select(.is_floating==true and .workspace_id==$WORKSPACE_ID) | .id"
}

#=============================================================================
# FUNCTION: cleanup_state_file
# Description: Removes closed windows from the state file
# Parameters:
#    $1 - List of currently active floating windows
#=============================================================================
cleanup_state_file() {
  local actual_floating="$1"
  
  if [ ! -s "$STATE_FILE" ]; then
      return
  fi
  
  local tmp_state
  tmp_state=$(mktemp)
  
  while IFS= read -r line; do
      local id_in_file
      id_in_file=$(echo "$line" | cut -d: -f1)
      
      if echo "$actual_floating" | grep -q "^${id_in_file}$"; then
          echo "$line" >> "$tmp_state"
      fi
  done < "$STATE_FILE"
  
  mv "$tmp_state" "$STATE_FILE"
}

#=============================================================================
# FUNCTION: calculate_window_dimensions
# Description: Calculates dynamic window height and X position
# Parameters:
#    $1 - Window count
# Returns: Sets WIN_H and X global variables
#=============================================================================
calculate_window_dimensions() {
  local count="$1"
  local slots="$BASE_SLOTS"
  
  # Increase slots if window count exceeds default
  if [ "$count" -gt "$BASE_SLOTS" ]; then
      slots="$count"
  fi
  
  # Calculate available vertical space
  local total_gaps_space=$(( (slots - 1) * GAP_BETWEEN ))
  local available_h=$(( SCREEN_H - TOP_MARGIN - BOTTOM_MARGIN - total_gaps_space ))
  
  # Export calculated dimensions
  WIN_H=$(( available_h / slots ))
  X=$(( SCREEN_W - WIN_W - SIDE_MARGIN ))
}

#=============================================================================
# FUNCTION: position_window
# Description: Moves and resizes a specific window
# Parameters:
#    $1 - Window ID
#    $2 - X position
#    $3 - Y position
#=============================================================================
position_window() {
  local id="$1"
  local x="$2"
  local y="$3"
  
  niri msg action set-window-width "$WIN_W" --id "$id" 2>/dev/null || true
  niri msg action set-window-height "$WIN_H" --id "$id" 2>/dev/null || true
  niri msg action move-floating-window --id "$id" --x "$x" --y "$y" 2>/dev/null || true
}

#=============================================================================
# FUNCTION: reorder_sidebar
# Description: Syncs state and updates all window positions on screen
#=============================================================================
reorder_sidebar() {
  local actual_floating
  actual_floating=$(get_floating_windows)

  cleanup_state_file "$actual_floating"

  local sorted_ids
  sorted_ids=$(grep -Ff <(echo "$actual_floating") "$STATE_FILE" 2>/dev/null | cut -d: -f1 || true)

  local count
  count=$(echo "$sorted_ids" | grep -c . || echo 0)

  if [ "$count" -eq 0 ]; then
      > "$FLAG_FILE"
      echo "visible" > "$HIDE_STATE_FILE"
      return
  fi

  calculate_window_dimensions "$count"

  local order
  order=$(cat "$ORDER_STATE_FILE" 2>/dev/null || echo "bottom-up")

  > "$FLAG_FILE"
  local index=0

  while IFS= read -r id; do
      [ -z "$id" ] && continue

      local pos_y
      if [ "$order" = "top-down" ]; then
          pos_y=$(( TOP_MARGIN + index * (WIN_H + GAP_BETWEEN) ))
      else
          pos_y=$(( SCREEN_H - BOTTOM_MARGIN - WIN_H - index * (WIN_H + GAP_BETWEEN) ))
      fi

      position_window "$id" "$X" "$pos_y"
      echo "${id}:${X}:${pos_y}" >> "$FLAG_FILE"

      index=$((index + 1))
  done <<< "$sorted_ids"

  echo "visible" > "$HIDE_STATE_FILE"
}


#=============================================================================
# FUNCTION: toggle_float
# Description: Toggles focused window between sidebar and tiling mode
#=============================================================================
toggle_float() {
  local focused_json
  focused_json=$(get_focused_window)
  
  local focused_id
  focused_id=$(echo "$focused_json" | jq -r '.id')
  
  if [ -z "$focused_id" ] || [ "$focused_id" = "null" ]; then
      echo "Error: No focused window"
      return 1
  fi
  
  if grep -q "^$focused_id:" "$STATE_FILE" 2>/dev/null; then
      # Restore to tiling mode
      local old_dim
      old_dim=$(grep "^$focused_id:" "$STATE_FILE")
      
      local old_w old_h
      old_w=$(echo "$old_dim" | cut -d: -f2)
      old_h=$(echo "$old_dim" | cut -d: -f3)
      
      # Remove from state
      sed -i "/^$focused_id:/d" "$STATE_FILE"
      
      # Action: Move to tiling
      niri msg action move-window-to-tiling
      
      # Restore original dimensions
      [ "$old_w" != "null" ] && niri msg action set-column-width "$old_w" 2>/dev/null || true
      [ "$old_h" != "null" ] && niri msg action set-window-height "$old_h" 2>/dev/null || true
      
      reorder_sidebar
      echo "✓ Window removed from sidebar"
  else
      # Add to sidebar
      local orig_w orig_h
      orig_w=$(echo "$focused_json" | jq -r '.layout.window_size[0] // 800')
      orig_h=$(echo "$focused_json" | jq -r '.layout.window_size[1] // 600')
      
      # Save original size
      echo "$focused_id:$orig_w:$orig_h" >> "$STATE_FILE"
      
      # Action: Move to floating
      niri msg action move-window-to-floating
      
      # Refresh sidebar
      reorder_sidebar
      
      # Keep focus on the window
      niri msg action focus-window --id "$focused_id" 2>/dev/null || true
      
      echo "✓ Window added to sidebar"
  fi
}

#=============================================================================
# FUNCTION: toggle_hide
# Description: Hides/Shows the entire sidebar by pushing windows off-screen
#=============================================================================
toggle_hide() {

  # Save current hide state
  local previous_state
  previous_state=$(cat "$HIDE_STATE_FILE" 2>/dev/null || echo "visible")

  # Reorder sidebar to fix any desync without breaking hide/show state
  local actual_floating
  actual_floating=$(get_floating_windows)

  if [ -n "$actual_floating" ]; then
      reorder_sidebar
      # reorder_sidebar forces visible, restore previous state
      echo "$previous_state" > "$HIDE_STATE_FILE"
  fi

  if [ ! -s "$FLAG_FILE" ]; then
      echo "No floating windows detected"
      return 0
  fi

  local current_state
  current_state=$(cat "$HIDE_STATE_FILE")

  if [ "$current_state" = "visible" ]; then
      # Hide: push windows off-screen
      while IFS=: read -r id x y; do
          [ -z "$id" ] && continue
          local new_x=$(( SCREEN_W - VISIBLE_SLIVER ))
          niri msg action move-floating-window --id "$id" --x "$new_x" --y "$y" 2>/dev/null || true
      done < "$FLAG_FILE"

      echo "hidden" > "$HIDE_STATE_FILE"
      echo "✓ Sidebar hidden"
  else
      # Show: restore original positions
      while IFS=: read -r id x y; do
          [ -z "$id" ] && continue
          niri msg action move-floating-window --id "$id" --x "$x" --y "$y" 2>/dev/null || true
      done < "$FLAG_FILE"

      echo "visible" > "$HIDE_STATE_FILE"
      echo "✓ Sidebar visible"
  fi
}


#=============================================================================
# FUNCTION: flip_sidebar_order
# Description: Flip the order of the sidebar
#=============================================================================

flip_sidebar_order() {
  local current
  current=$(cat "$ORDER_STATE_FILE" 2>/dev/null || echo "bottom-up")

  if [ "$current" = "bottom-up" ]; then
      echo "top-down" > "$ORDER_STATE_FILE"
  else
      echo "bottom-up" > "$ORDER_STATE_FILE"
  fi

  reorder_sidebar
  echo "✓ Sidebar order flipped"
}

sidebar_listener() {
  niri msg event-stream | while read -r line; do
    case "$line" in
      *"Window closed"*)
        # Window was closed → resync sidebar
        reorder_sidebar
        ;;
    esac
  done
}



#=============================================================================
# FUNCTION: show_usage
# Description: Displays help information
#=============================================================================
show_usage() {
  cat << EOF
Usage: $0 {toggle|hide|help}

Commands:
toggle    Toggle focused window between sidebar and tiling mode
hide      Hide/show the entire sidebar
flip      Flip sidebar order (top-down / bottom-up)
reorder   Syncs state and updates all window positions on screen
listen    Start a listener to reorder the sidebar
help      Show this help message

Examples:
$0 toggle    # Add/remove current window
$0 hide      # Show/hide all floating windows
EOF
}

#=============================================================================
# MAIN ENTRY POINT
#=============================================================================
main() {
  ensure_files_exist
  
  case "${1:-}" in
      toggle)
          toggle_float
          ;;
      hide)
          toggle_hide
          ;;
      flip)
          flip_sidebar_order
          ;;
      reorder)    
          reorder_sidebar
          ;;
      listen)
          sidebar_listener
          ;;
      help|--help|-h)
          show_usage
          ;;
      *)
          echo "❌ Invalid command"
          echo ""
          show_usage
          exit 1
          ;;
  esac
}

main "$@"
