local obj = {}
obj.__index = obj

-- 기본 설정
obj.name = "wiggle"
obj.version = "1.0.0"
obj.license = "MIT"

-- 외부에서 변경 가능한 설정값
obj.maxMoveRange = 500 -- 최대 픽셀 범위
obj.minInterval = 5 -- 최소 간격 (초)
obj.maxInterval = 20 -- 최대 간격 (초)
obj.steps = 30 -- 이동을 나눌 단계 수
obj.stepDelay = 0.01 -- 단계 간 딜레이 (초 단위)

local mouseJiggleMenubar = hs.menubar.new()
local jiggleTimer = nil
local isJiggling = false
local animationTimers = {}
local inited = false
local hotkeyObjects = {}

-- Ease-in-out 함수: 시작과 끝에서 느리고 중간에서 빠르게
local function easeInOutQuad(t)
	if t < 0.5 then
		return 2 * t * t
	else
		return 1 - ((-2 * t + 2) ^ 2) / 2
	end
end

-- 화면 경계 내로 좌표 제한
local function clampToScreen(x, y)
	local screen = hs.mouse.getCurrentScreen()
	if not screen then
		screen = hs.screen.mainScreen()
	end
	local frame = screen:frame()

	local clampedX = math.max(frame.x, math.min(x, frame.x + frame.w - 1))
	local clampedY = math.max(frame.y, math.min(y, frame.y + frame.h - 1))

	return clampedX, clampedY
end

-- 진행 중인 애니메이션 타이머 정리
local function clearAnimationTimers()
	for _, timer in ipairs(animationTimers) do
		if timer and timer:running() then
			timer:stop()
		end
	end
	animationTimers = {}
end

-- 비동기 방식으로 마우스 부드럽게 이동
local function jiggleMouse()
	clearAnimationTimers()

	local currentPos = hs.mouse.absolutePosition()
	-- moveRange를 0에서 maxMoveRange 사이의 랜덤값으로 결정
	local moveRange = math.random(0, obj.maxMoveRange)
	local targetX = currentPos.x + math.random(-moveRange, moveRange)
	local targetY = currentPos.y + math.random(-moveRange, moveRange)

	-- 화면 경계 내로 목표 좌표 제한
	targetX, targetY = clampToScreen(targetX, targetY)

	local startX, startY = currentPos.x, currentPos.y

	-- 비동기 타이머로 각 단계 실행 (메인 스레드 블로킹 방지)
	for step = 1, obj.steps do
		local timer = hs.timer.doAfter(step * obj.stepDelay, function()
			local progress = step / obj.steps
			local easedProgress = easeInOutQuad(progress)
			local newX = startX + (targetX - startX) * easedProgress
			local newY = startY + (targetY - startY) * easedProgress
			hs.mouse.absolutePosition({ x = newX, y = newY })
			hs.eventtap.event.newMouseEvent(hs.eventtap.event.types.mouseMoved, { x = newX }):post()
		end)
		table.insert(animationTimers, timer)
	end
end

-- 랜덤 간격으로 다음 jiggle 예약
local function scheduleNextJiggle()
	if not isJiggling then
		return
	end
	local interval = math.random(obj.minInterval, obj.maxInterval)
	jiggleTimer = hs.timer.doAfter(interval, function()
		jiggleMouse()
		scheduleNextJiggle()
	end)
end

local function startJiggle()
	if not isJiggling then
		isJiggling = true
		jiggleMouse() -- 즉시 한 번 실행
		scheduleNextJiggle()
		mouseJiggleMenubar:setTitle("●") -- ON: 채운 원
	end
end

local function stopJiggle()
	isJiggling = false
	if jiggleTimer then
		jiggleTimer:stop()
		jiggleTimer = nil
	end
	mouseJiggleMenubar:setTitle("○") -- OFF: 빈 원
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

-- 핫키 바인딩 (init.lua에서 호출)
-- mapping 예: { toggle = { "ctrl", "shift", "alt", "cmd", "a" } } (마지막이 키, 나머지가 수식자)
function obj:bindHotkeys(mapping)
	self:init()
	for _, spec in pairs(hotkeyObjects) do
		if spec and spec.delete then
			spec:delete()
		end
	end
	hotkeyObjects = {}
	if not mapping or type(mapping) ~= "table" then
		return
	end
	for name, spec in pairs(mapping) do
		if name == "toggle" and type(spec) == "table" and #spec >= 2 then
			local key = spec[#spec]
			local mods = {}
			for i = 1, #spec - 1 do
				mods[i] = spec[i]
			end
			local obj = hs.hotkey.bind(mods, key, function()
				self:toggle()
			end)
			hotkeyObjects[name] = obj
			break
		end
	end
end

return obj
