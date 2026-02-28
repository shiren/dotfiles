-- luacheck: globals hs spoon

local function loadSpoonSafe(name, fn)
	local ok, err = pcall(fn)
	if not ok then
		hs.notify.new({ title = "Hammerspoon", informativeText = name .. " 로드 실패: " .. tostring(err) }):send()
	end
end

loadSpoonSafe("wiggle", function()
	hs.loadSpoon("wiggle")
	spoon.wiggle:init()
	spoon.wiggle:bindHotkeys({
		toggle = { "ctrl", "shift", "alt", "cmd", "a" },
	})
end)

loadSpoonSafe("VolumeOSC", function()
	hs.loadSpoon("VolumeOSC")
	spoon.VolumeOSC:start()
end)

loadSpoonSafe("BrowserRouter", function()
	hs.loadSpoon("BrowserRouter")
	spoon.BrowserRouter:start()
end)
