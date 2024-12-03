import os
import sys
import json
import socket
from typing import Callable

xdg_runtime_dir = os.getenv("XDG_RUNTIME_DIR")
hyprland_instance_signature = os.getenv("HYPRLAND_INSTANCE_SIGNATURE")

socket_path = lambda socket: os.path.join(
    xdg_runtime_dir, 'hypr', hyprland_instance_signature, socket
)

def hyprctl(command: str, args: list[str] = []) -> str|dict:
    # TODO Use with instead of try
    try:
        sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        sock.connect(socket_path('.socket.sock'))

        if args:
            command = f"{' '.join(args)}/{command}"

        sock.sendall(command.encode('utf-8'))

        res = sock.recv(8192).decode('utf-8')

    finally:
        sock.close()

    if "-j" in args:
        return json.loads(res)

    return res


def hyprlisten(filter: list[str] = [], callback: Callable = lambda x: print(x, end='')) -> None:
    with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as sock:
        sock.connect(socket_path('.socket2.sock'))

        # Wrap the socket in a file-like object for line-by-line reading
        with sock.makefile() as sock_file:
            for line in sock_file:
                event = line.split('>>', 1)[0]
                if filter and event not in filter:
                    continue
                callback(line)


def detect_gamemode(event: str) -> None:
    workspaces = hyprctl("workspaces", ["-j"])

    try:
        # Workspace 10 is occupied
        ws10 = min(filter(lambda w: w["name"] == "10", workspaces))

        if ws10['windows'] <= 0:
            raise ValueError

        disable_effects()
    except ValueError:
        # Workspace 10 is empty
        if has_effects():
            return

        restore_effects()


def has_effects() -> bool:
    return hyprctl("getoption animations:enabled", ["-j"])["int"] == 1


def disable_effects() -> None:
    hyprctl("keyword animations:enabled 0")
    hyprctl("keyword decoration:drop_shadow 0")
    hyprctl("keyword decoration:blur:enabled 0")
    hyprctl("keyword general:gaps_in 0")
    hyprctl("keyword general:gaps_out 0")
    hyprctl("keyword general:border_size 1")
    hyprctl("keyword general:allow_tearing true")
    hyprctl("keyword decoration:rounding 0")
    hyprctl("keyword plugin:dynamic-cursors:enabled false")


def restore_effects() -> None:
    hyprctl("reload")


def toggle_effects() -> None:
    if has_effects():
        disable_effects()
    else:
        restore_effects()


if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == "toggle":
        toggle_effects()
    else:
        hyprlisten(["openwindow", "closewindow"], detect_gamemode)
