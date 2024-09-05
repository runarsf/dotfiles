import os
import json
import socket

xdg_runtime_dir = os.getenv("XDG_RUNTIME_DIR")
hyprland_instance_signature = os.getenv("HYPRLAND_INSTANCE_SIGNATURE")

socket_path = f"{xdg_runtime_dir}/hypr/{hyprland_instance_signature}/.socket.sock"

def hyprctl(command: str, args: list[str] = ["-j"]) -> str|dict:
    try:
        sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        sock.connect(socket_path)

        if args:
            command = f"{''.join(args)}/{command}"
        sock.sendall(command.encode('utf-8'))

        res = sock.recv(8192).decode('utf-8')

    finally:
        sock.close()

    if "-j" in args:
        return json.loads(res)

    return res

gravity = 30

client = hyprctl("activewindow")

if not client["floating"]:
    exit(0)

workspace_id = hyprctl("activeworkspace")["id"]
client_pid = client["pid"]
clients = list(filter(lambda c: c["workspace"]["id"] == workspace_id and c["pid"] != client_pid, hyprctl("clients")))

def get_geometry(client: dict) -> dict:
    return {
        "n": client["at"][1],
        "s": client["at"][1] + client["size"][1],
        "w": client["at"][0],
        "e": client["at"][0] + client["size"][0],
    }

client_geometry = get_geometry(client)
clients_geometry = list(map(get_geometry, clients))
closest_edges = {"n": None, "s": None, "w": None, "e": None}

# monitor_id = list(filter(lambda w: w["id"] == client["workspace"]["id"], hyprctl("workspaces")))[0]["monitorID"]
# monitor = list(filter(lambda m: m["id"] == monitor_id, hyprctl("monitors")))[0]
monitor = list(filter(lambda m: m["focused"], hyprctl("monitors")))[0]

pad = {cardinal: 0 for cardinal in closest_edges.keys()}

gaps = hyprctl("getoption general:gaps_out")
if "int" in gaps:
    for cardinal in pad.keys():
        gaps[cardinal] = gaps["int"]
else:
    gaps = [int(g) for g in gaps["custom"].split(" ")]
    pad["n"] += gaps[1]
    pad["w"] += gaps[3]
    pad["s"] += gaps[0]
    pad["e"] += gaps[2]

border = hyprctl("getoption general:border_size")["int"]
pad = {cardinal: pad[cardinal] + border for cardinal in pad.keys()}
pad["n"] += monitor["reserved"][1]
pad["s"] += monitor["reserved"][3]
pad["w"] += monitor["reserved"][0]
pad["e"] += monitor["reserved"][2]

if closest_edges["w"] is None and client_geometry["w"] < (w:=(monitor["x"] + pad["w"])):
    closest_edges["w"] = w
if closest_edges["e"] is None and client_geometry["e"] > (e:=(monitor["x"] + monitor["width"] - pad["w"])):
    closest_edges["e"] = e
if closest_edges["n"] is None and client_geometry["n"] < (n:=(monitor["y"] + pad["n"])):
    closest_edges["n"] = n
if closest_edges["s"] is None and client_geometry["s"] > (s:=(monitor["y"] + monitor["height"] - pad["s"])):
    closest_edges["s"] = s

for other in clients_geometry:
    for edge in closest_edges.keys():
        if abs(client_geometry[edge] - other[edge]) <= gravity:
            if closest_edges[edge] is None or abs(client_geometry[edge] - closest_edges[edge]) > abs(client_geometry[edge] - other[edge]):
                closest_edges[edge] = other[edge]

def snap_to_closest_edges(window, closest_edges):
    x, y, w, h = window['at'][0], window['at'][1], window['size'][0], window['size'][1]

    if closest_edges["w"] is not None:
        x = closest_edges['w']
    if closest_edges["e"] is not None:
        if closest_edges["w"] is not None:
            w = closest_edges['e'] - closest_edges['w']
        else:
            x = closest_edges['e'] - w
    if closest_edges["n"] is not None:
        y = closest_edges['n']
    if closest_edges["s"] is not None:
        if closest_edges["n"] is not None:
            h = closest_edges['s'] - closest_edges['n']
        else:
            y = closest_edges['s'] - h

    os.system(f'hyprctl dispatch movewindowpixel -- exact "{x}" "{y}",pid:{client_pid}')
    os.system(f'hyprctl dispatch resizewindowpixel -- exact "{w}" "{h}",pid:{client_pid}')

snap_to_closest_edges(client, closest_edges)
