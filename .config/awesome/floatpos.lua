floatgeoms = {}

tag.connect_signal("property::layout", function(t)
    for k, c in ipairs(t:clients()) do
        if ((awful.layout.get(mouse.screen) == awful.layout.suit.floating) or (awful.client.floating.get(c) == true)) then
            c:geometry(floatgeoms[c.window])
        end
    endclient.connect_signal("unmanage", function(c) floatgeoms[c.window] = nil end)
end)

client.connect_signal("property::geometry", function(c)
    if ((awful.layout.get(mouse.screen) == awful.layout.suit.floating) or (awful.client.floating.get(c) == true)) then
        floatgeoms[c.window] = c:geometry()
    end
end)

client.connect_signal("unmanage", function(c) floatgeoms[c.window] = nil end)    

client.connect_signal("manage", function(c)
    if ((awful.layout.get(mouse.screen) == awful.layout.suit.floating) or (awful.client.floating.get(c) == true)) then
        floatgeoms[c.window] = c:geometry()
    end
end)
