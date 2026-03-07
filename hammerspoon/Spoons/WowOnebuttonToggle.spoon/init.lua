local obj = {}
obj.__index = obj

obj.name = "WowOnebuttonToggle"
obj.version = "1.0.0"
obj.license = "MIT"

obj.wowBundleIDs = {
	["com.blizzard.worldofwarcraft"] = true,
	["com.blizzard.worldofwarcraftclassic"] = true,
}
obj.wowAppName = "World of Warcraft"
obj.mouseButtonNumber = 4
obj.intervalSeconds = 0.9
obj.clickDelayMicros = 1000
obj.alertDuration = 0.4

local tap
local timer
local enabled = false
local suppressNextToggle = false

local function isWoWFrontmost(self)
	local app = hs.application.frontmostApplication()
	if not app then
		return false
	end

	local bundleID = app:bundleID()
	if bundleID and self.wowBundleIDs[bundleID] then
		return true
	end

	local appName = app:name()
	return appName and appName:find(self.wowAppName, 1, true) ~= nil
end

local function clickMouse5(self)
	local pos = hs.mouse.absolutePosition()
	suppressNextToggle = true
	hs.eventtap.otherClick(pos, self.clickDelayMicros, self.mouseButtonNumber)
end

local function showState(self)
	if not self.alertDuration or self.alertDuration <= 0 then
		return
	end
	hs.alert.closeAll()
	hs.alert.show(enabled and "Mouse5 Loop ON" or "Mouse5 Loop OFF", self.alertDuration)
end

local function setEnabled(self, value)
	if enabled == value then
		return
	end

	enabled = value
	if enabled then
		timer = hs.timer.doEvery(self.intervalSeconds, function()
			if not isWoWFrontmost(self) then
				return
			end
			clickMouse5(self)
		end)
	else
		if timer then
			timer:stop()
			timer = nil
		end
	end

	showState(self)
end

local function toggle(self)
	setEnabled(self, not enabled)
end

function obj:start()
	if tap then
		return
	end

	tap = hs.eventtap.new({ hs.eventtap.event.types.otherMouseDown }, function(event)
		if event:getProperty(hs.eventtap.event.properties.mouseEventButtonNumber) ~= self.mouseButtonNumber then
			return false
		end
		if suppressNextToggle then
			suppressNextToggle = false
			return false
		end
		if not isWoWFrontmost(self) then
			return false
		end
		toggle(self)
		return false
	end)

	tap:start()
end

function obj:stop()
	setEnabled(self, false)
	if tap then
		tap:stop()
		tap = nil
	end
end

return obj
