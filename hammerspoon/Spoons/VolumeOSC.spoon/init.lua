-- VolumeOSC.spoon – OSC 기반 볼륨 제어 (RME TotalMix 등)
-- 미디어 키(볼륨 업/다운)를 감지해 UDP OSC로 원격 볼륨 값을 전송합니다.

local obj = {}
obj.__index = obj

obj.name = "VolumeOSC"
obj.version = "1.0.0"
obj.license = "MIT"

-- 설정 (init 전에 변경 가능)
obj.host = "127.0.0.1"
obj.port = 7001
obj.paths = { "/1/mastervolume", "/1/busOutput/1/volume" }
obj.step = 0.02 -- 볼륨 업/다운 시 증감량 (0.0 ~ 1.0)
obj.initialVolume = 0.5 -- 초기 볼륨 (0.0 ~ 1.0)
obj.alertDuration = 0.5 -- 볼륨 알림 표시 시간(초), 1이면 표시 안 함

local currentVol
local mediaWatcher

-- OSC 메시지 전송 (Path + Type Tag ",f" + Float big-endian)
local function sendOSC(self, value)
	if not self.paths or #self.paths == 0 then
		return
	end
	local socket = hs.socket.udp.new()
	if not socket then
		return
	end
	for _, path in ipairs(self.paths) do
		local message = path .. string.rep("\0", 4 - (#path % 4))
		message = message .. ",f\0\0"
		message = message .. string.pack(">f", value)
		socket:send(message, self.host, self.port)
	end
	socket:close()
end

function obj:init()
	currentVol = self.initialVolume
	mediaWatcher = hs.eventtap.new({ hs.eventtap.event.types.systemDefined }, function(event)
		local systemKey = event:systemKey()
		if not systemKey.down then
			return false
		end
		if systemKey.key == "SOUND_UP" then
			currentVol = math.min(1.0, currentVol + self.step)
		elseif systemKey.key == "SOUND_DOWN" then
			currentVol = math.max(0.0, currentVol - self.step)
		else
			return false
		end

		sendOSC(self, currentVol)

		if self.alertDuration and self.alertDuration > 0 then
			hs.alert.closeAll()
			hs.alert.show("Vol: " .. math.floor(currentVol * 100) .. "%", self.alertDuration)
		end
		return false
	end)
end

function obj:start()
	if not mediaWatcher then
		self:init()
	end
	mediaWatcher:start()
end

function obj:stop()
	if mediaWatcher then
		mediaWatcher:stop()
	end
end

-- 현재 볼륨 값 (0.0 ~ 1.0). 설정만 하고 싶을 때 사용 가능.
function obj:getVolume()
	return currentVol or self.initialVolume
end

function obj:setVolume(value)
	local v = tonumber(value)
	if v then
		currentVol = math.max(0.0, math.min(1.0, v))
		sendOSC(self, currentVol)
	end
end

return obj
