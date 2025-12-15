local wezterm = require 'wezterm'

local config = wezterm.config_builder()

config.automatically_reload_config = true

config.audible_bell = "Disabled"

config.use_ime = true

config.color_scheme = 'Ayu Mirage'

config.font = wezterm.font("JetBrains Mono NL", {weight="Medium", stretch="Normal", style="Normal"})

return config
