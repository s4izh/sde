apps = {
    "kanshi",
    "waybar",
    "swayosd-server",
    "swaync",
    "nm-applet --indicator",
    -- "swaybg -i /path/to/wall.png"
}

function run_bg(cmd)
    local p_name = cmd:match("%S+")
    os.execute("pkill " .. p_name .. " > /dev/null 2>&1")
    os.execute(cmd .. " &")
end

for _, app in ipairs(apps) do
    run_bg(app)
end
