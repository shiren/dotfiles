-- luacheck: globals hs spoon

local function loadSpoonSafe(name, fn)
	local ok, err = pcall(fn)
	if not ok then
		hs.notify.new({ title = "Hammerspoon", informativeText = name .. " 로드 실패: " .. tostring(err) }):send()
	end
end

-- IOPlatformUUID 확인:
-- ioreg -rd1 -c IOPlatformExpertDevice | awk -F\" '/IOPlatformUUID/{print $4}'
local platformUUID = hs.execute("ioreg -rd1 -c IOPlatformExpertDevice | awk -F\\\" '/IOPlatformUUID/{print $4}'"):gsub("%s+$", "")
local disableSpoonByUUID = {
	["AC201633-05AE-58CA-BCB4-4AF5A94B955B"] = {
		"BrowserRouter",
	},
}

local function isSpoonEnabledOnThisMac(spoonName)
	local disabledSpoonList = disableSpoonByUUID[platformUUID]
	if disabledSpoonList == nil then
		return true
	end

	for _, disabledSpoonName in ipairs(disabledSpoonList) do
		if disabledSpoonName == spoonName then
			return false
		end
	end

	return true
end

local function loadConfiguredSpoon(spoonName, fn)
	if isSpoonEnabledOnThisMac(spoonName) then
		loadSpoonSafe(spoonName, fn)
	end
end

loadConfiguredSpoon("wiggle", function()
	hs.loadSpoon("wiggle")
	spoon.wiggle:init()
	spoon.wiggle:bindHotkeys({
		toggle = { "ctrl", "shift", "alt", "cmd", "a" },
	})
end)

loadConfiguredSpoon("VolumeOSC", function()
	hs.loadSpoon("VolumeOSC")
	spoon.VolumeOSC:start()
end)

loadConfiguredSpoon("BrowserRouter", function()
	hs.loadSpoon("BrowserRouter")
	spoon.BrowserRouter:start()
end)

loadConfiguredSpoon("WowOnebuttonToggle", function()
	hs.loadSpoon("WowOnebuttonToggle")
	spoon.WowOnebuttonToggle:start()
end)
