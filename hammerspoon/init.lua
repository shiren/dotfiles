-- luacheck: globals hs spoon

local ok, err

ok, err = pcall(function()
	hs.loadSpoon("wiggle")
	spoon.wiggle:init()
	spoon.wiggle:bindHotkeys({
		toggle = { "ctrl", "shift", "alt", "cmd", "a" },
	})
end)
if not ok then
	hs.notify.new({ title = "Hammerspoon", informativeText = "wiggle 로드 실패: " .. tostring(err) }):send()
end

ok, err = pcall(function()
	hs.loadSpoon("VolumeOSC")
	spoon.VolumeOSC:start()
end)
if not ok then
	hs.notify.new({ title = "Hammerspoon", informativeText = "VolumeOSC 로드 실패: " .. tostring(err) }):send()
end
