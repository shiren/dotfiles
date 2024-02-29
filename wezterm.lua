-- Pull in the wezterm API
local wezterm = require("wezterm")

-- This will hold the configuration.
local config = wezterm.config_builder()

-- This is where you actually apply your config choices

-- For example, changing the color scheme:
config.color_scheme = "Rosé Pine Moon (base16)"

config.font_size = 14
config.font = wezterm.font_with_fallback({ "JetBrains Mono", "NanumGothic" })

config.enable_tab_bar = false

-- and finally, return the configuration to wezterm
return config
