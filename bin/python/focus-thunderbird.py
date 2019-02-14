#!/usr/local/bin/python3

import i3ipc

i3 = i3ipc.Connection()

nodes = []


def process_node(node):
    global nodes
    nodes.append(node)
    for sel_node in node.nodes:
        process_node(sel_node)


process_node(i3.get_tree())

wine_nodes = [x for x in nodes if x.window_class = "Thunderbird"]

wine_nodes[0].command("focus")
