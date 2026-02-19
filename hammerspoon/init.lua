-- luacheck: globals hs spoon

hs.loadSpoon("wiggle")

spoon.wiggle:bindHotkeys({
	toggle = { "cmd", "alt", "J" },
})

-- OSC 전송 함수 (UDP 엔진)
local function sendRME(value)
	local socket = hs.socket.udp.new()
	local ip = "127.0.0.1"
	local port = 7001

	-- 전송할 가능한 주소 리스트
	local paths = { "/1/mastervolume", "/1/busOutput/1/volume" }

	for _, path in ipairs(paths) do
		-- OSC 메시지 조립 (Path + Type Tag + Float Value)
		local message = path .. string.rep("\0", 4 - (#path % 4))
		message = message .. ",f\0\0"
		message = message .. string.pack(">f", value)

		socket:send(message, ip, port)
	end
	socket:close()
end

local currentVol = 0.5 -- 0.0 ~ 1.0 (50%)

-- 미디어 키 감지
mediaWatcher = hs.eventtap.new({ hs.eventtap.event.types.systemDefined }, function(event)
	local systemKey = event:systemKey()
	if systemKey.down then
		if systemKey.key == "SOUND_UP" then
			currentVol = math.min(1.0, currentVol + 0.02) -- 2%씩 정밀 조절
		elseif systemKey.key == "SOUND_DOWN" then
			currentVol = math.max(0.0, currentVol - 0.02)
		else
			return false
		end

		sendRME(currentVol)

		hs.alert.closeAll()
		hs.alert.show("TotalMix OSC Vol: " .. math.floor(currentVol * 100) .. "%", 0.5)
	end
	return false
end)

mediaWatcher:start()
print("🚀 TotalMix OSC Watcher Active (Port 7001)")
