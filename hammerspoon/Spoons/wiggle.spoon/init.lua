local obj = {}
obj.__index = obj

obj.name = "wiggle"
obj.version = "1.0.0"
obj.license = "MIT"

obj.maxMoveRange = 500
obj.minInterval = 5
obj.maxInterval = 20
obj.steps = 30
obj.stepDelay = 0.01

local mouseJiggleMenubar = hs.menubar.new()
local jiggleTimer
local isJiggling = false
local animationTimers = {}
local inited = false
local hotkeyObjects = {}

local function easeInOutQuad(t)
	if t < 0.5 then
		return 2 * t * t
	end
	return 1 - ((-2 * t + 2) ^ 2) / 2
end

local function clampToScreen(x, y)
	local screen = hs.mouse.getCurrentScreen() or hs.screen.mainScreen()
	local frame = screen:frame()
	return math.max(frame.x, math.min(x, frame.x + frame.w - 1)),
		math.max(frame.y, math.min(y, frame.y + frame.h - 1))
end

local function clearAnimationTimers()
	for _, timer in ipairs(animationTimers) do
		if timer and timer:running() then
			timer:stop()
		end
	end
	animationTimers = {}
end

local function postMouseMoved(x, y)
	local ev = hs.eventtap.event.newMouseEvent(hs.eventtap.event.types.mouseMoved, { x = x, y = y })
	if ev then
		ev:post()
	end
end

local function jiggleMouse()
	clearAnimationTimers()
	local currentPos = hs.mouse.absolutePosition()
	local moveRange = math.random(0, obj.maxMoveRange)
	local targetX, targetY = clampToScreen(
		currentPos.x + math.random(-moveRange, moveRange),
		currentPos.y + math.random(-moveRange, moveRange)
	)
	local startX, startY = currentPos.x, currentPos.y

	for step = 1, obj.steps do
		local timer = hs.timer.doAfter(step * obj.stepDelay, function()
			local progress = step / obj.steps
			local eased = easeInOutQuad(progress)
			local newX = startX + (targetX - startX) * eased
			local newY = startY + (targetY - startY) * eased
			hs.mouse.absolutePosition({ x = newX, y = newY })
			postMouseMoved(newX, newY)
		end)
		table.insert(animationTimers, timer)
	end
end

local function scheduleNextJiggle()
	if not isJiggling then
		return
	end
	jiggleTimer = hs.timer.doAfter(math.random(obj.minInterval, obj.maxInterval), function()
		jiggleMouse()
		scheduleNextJiggle()
	end)
end

local function startJiggle()
	if isJiggling then
		return
	end
	isJiggling = true
	jiggleMouse()
	scheduleNextJiggle()
	mouseJiggleMenubar:setTitle("●")
end

local function stopJiggle()
	isJiggling = false
	if jiggleTimer then
		jiggleTimer:stop()
		jiggleTimer = nil
	end
	mouseJiggleMenubar:setTitle("○")
end

function obj:init()
	if inited then
		return
	end
	inited = true
	mouseJiggleMenubar:setClickCallback(function()
		if isJiggling then
			stopJiggle()
		else
			startJiggle()
		end
	end)
	mouseJiggleMenubar:setTitle("○")
end

function obj:start()
	self:init()
	startJiggle()
end

function obj:stop()
	stopJiggle()
end

function obj:toggle()
	if isJiggling then
		self:stop()
	else
		self:start()
	end
end

function obj:bindHotkeys(mapping)
	self:init()
	for _, hotkeyObj in pairs(hotkeyObjects) do
		if hotkeyObj and hotkeyObj.delete then
			hotkeyObj:delete()
		end
	end
	hotkeyObjects = {}

	local spec = mapping and mapping.toggle
	if type(spec) ~= "table" or #spec < 2 then
		return
	end
	local key = spec[#spec]
	local mods = {}
	for i = 1, #spec - 1 do
		mods[i] = spec[i]
	end
	hotkeyObjects.toggle = hs.hotkey.bind(mods, key, function()
		self:toggle()
	end)
end

return obj
