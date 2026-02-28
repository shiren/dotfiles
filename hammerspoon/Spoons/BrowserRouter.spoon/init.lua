local obj = {}
obj.__index = obj

obj.name = "BrowserRouter"
obj.version = "1.0.0"
obj.license = "MIT"

obj.menuTitle = "BR"

local menuBar
local previousHttpCallback
local spoonDir
local rulesPath
local config = {}
local activeChoosers = {}

local knownBrowsers = {
	{
		name = "Google Chrome",
		bundleID = "com.google.Chrome",
		userDataDir = os.getenv("HOME") .. "/Library/Application Support/Google/Chrome",
		executable = "Google Chrome",
	},
	{
		name = "Google Chrome Beta",
		bundleID = "com.google.Chrome.beta",
		userDataDir = os.getenv("HOME") .. "/Library/Application Support/Google/Chrome Beta",
		executable = "Google Chrome Beta",
	},
	{
		name = "Chromium",
		bundleID = "org.chromium.Chromium",
		userDataDir = os.getenv("HOME") .. "/Library/Application Support/Chromium",
		executable = "Chromium",
	},
	{
		name = "Brave Browser",
		bundleID = "com.brave.Browser",
		userDataDir = os.getenv("HOME") .. "/Library/Application Support/BraveSoftware/Brave-Browser",
		executable = "Brave Browser",
	},
	{
		name = "Microsoft Edge",
		bundleID = "com.microsoft.edgemac",
		userDataDir = os.getenv("HOME") .. "/Library/Application Support/Microsoft Edge",
		executable = "Microsoft Edge",
	},
	{
		name = "Arc",
		bundleID = "company.thebrowser.Browser",
	},
	{
		name = "Safari",
		bundleID = "com.apple.Safari",
	},
}

local function getSpoonDir()
	local source = debug.getinfo(1, "S").source
	if source:sub(1, 1) == "@" then
		return source:sub(2):match("^(.*)/init%.lua$")
	end
	return nil
end

local function fileExists(path)
	local attrs = hs.fs.attributes(path)
	return attrs and attrs.mode == "file"
end

local function readFile(path)
	local file = io.open(path, "r")
	if not file then
		return nil
	end
	local content = file:read("*a")
	file:close()
	return content
end

local function writeFile(path, content)
	local file, err = io.open(path, "w")
	if not file then
		return false, err
	end
	file:write(content)
	file:close()
	return true
end

local function appendFile(path, content)
	local file = io.open(path, "a")
	if not file then
		return false
	end
	file:write(content)
	file:close()
	return true
end

local function browserMetaByName(name)
	local target = string.lower(tostring(name or ""))
	for _, browser in ipairs(knownBrowsers) do
		if string.lower(browser.name) == target then
			return browser
		end
	end
	return nil
end

local function isValidAppBundle(path)
	if type(path) ~= "string" or path == "" then
		return false
	end
	local attrs = hs.fs.attributes(path)
	if not attrs or attrs.mode ~= "directory" then
		return false
	end
	return string.match(path, "%.app/?$") ~= nil
end

local function isBrowserInstalled(browser)
	local appPath = hs.application.pathForBundleID(browser.bundleID)
	if isValidAppBundle(appPath) then
		return true
	end

	local home = os.getenv("HOME") or ""
	local fallbackPaths = {
		"/Applications/" .. browser.name .. ".app",
		home .. "/Applications/" .. browser.name .. ".app",
	}
	for _, path in ipairs(fallbackPaths) do
		if isValidAppBundle(path) then
			return true
		end
	end
	return false
end

local function appBundlePathForBrowser(browserName)
	local browser = browserMetaByName(browserName)
	if not browser then
		return nil
	end

	local appPath = hs.application.pathForBundleID(browser.bundleID)
	if isValidAppBundle(appPath) then
		return appPath
	end

	local home = os.getenv("HOME") or ""
	local fallbackPaths = {
		"/Applications/" .. browser.name .. ".app",
		home .. "/Applications/" .. browser.name .. ".app",
	}
	for _, path in ipairs(fallbackPaths) do
		if isValidAppBundle(path) then
			return path
		end
	end
	return nil
end

local function listInstalledBrowsers()
	local out = {}
	for _, browser in ipairs(knownBrowsers) do
		if isBrowserInstalled(browser) then
			table.insert(out, browser)
		end
	end
	return out
end

local function readProfileNamesFromLocalState(path)
	local localState = readFile(path .. "/Local State")
	if not localState or localState == "" then
		return {}
	end
	local decoded = hs.json.decode(localState)
	if type(decoded) ~= "table" or type(decoded.profile) ~= "table" or type(decoded.profile.info_cache) ~= "table" then
		return {}
	end

	local out = {}
	for profileDir, _ in pairs(decoded.profile.info_cache) do
		if type(profileDir) == "string" then
			out[profileDir] = true
		end
	end
	return out
end

local function readProfileNamesFromDirectory(path)
	local out = {}
	local ok, iterator, dirObj = pcall(hs.fs.dir, path)
	if not ok then
		return out
	end
	if not iterator then
		return out
	end
	for entry in iterator, dirObj do
		if entry ~= "." and entry ~= ".." then
			local fullPath = path .. "/" .. entry
			local attrs = hs.fs.attributes(fullPath)
			if attrs and attrs.mode == "directory" then
				if entry == "Default" or entry == "Guest Profile" or entry:match("^Profile %d+$") then
					out[entry] = true
				end
			end
		end
	end
	return out
end

local function sortedProfileList(profileSet)
	local list = {}
	for name, _ in pairs(profileSet) do
		table.insert(list, name)
	end
	table.sort(list, function(a, b)
		if a == "Default" then
			return true
		end
		if b == "Default" then
			return false
		end
		return a < b
	end)
	return list
end

local function profileChoicesForBrowser(browser)
	if not browser or not browser.userDataDir then
		return {
			{
				text = "프로파일 사용 안 함",
				subText = "이 브라우저는 프로파일 선택을 사용하지 않음",
			},
		}
	end

	local profiles = readProfileNamesFromLocalState(browser.userDataDir)
	local fromDirs = readProfileNamesFromDirectory(browser.userDataDir)
	for profileName, _ in pairs(fromDirs) do
		profiles[profileName] = true
	end

	local names = sortedProfileList(profiles)
	local choices = {
		{ text = "프로파일 사용 안 함", subText = "브라우저 기본 프로파일로 열기" },
	}
	for _, profileName in ipairs(names) do
		table.insert(choices, {
			text = profileName,
			subText = browser.name .. " profile",
		})
	end

	return choices
end

function obj:_showChooser(placeholder, choices, onPick)
	local chooser
	chooser = hs.chooser.new(function(choice)
		activeChoosers[chooser] = nil
		if not choice then
			return
		end
		local ok, err = pcall(onPick, choice)
		if not ok then
			print("BrowserRouter chooser callback error: " .. tostring(err))
			hs.alert.show("BrowserRouter: 선택 처리 오류")
		end
	end)
	chooser:placeholderText(placeholder)
	chooser:choices(choices)
	activeChoosers[chooser] = true
	chooser:show()
end

function obj:_chooseBrowser(onPick)
	local installed = listInstalledBrowsers()
	if #installed == 0 then
		hs.alert.show("BrowserRouter: 설치된 브라우저를 찾지 못했습니다")
		return
	end

	local choices = {}
	for _, browser in ipairs(installed) do
		table.insert(choices, {
			text = browser.name,
			subText = browser.userDataDir and "프로파일 선택 지원" or "프로파일 선택 미지원",
		})
	end
	table.sort(choices, function(a, b)
		return a.text < b.text
	end)

	self:_showChooser("브라우저 선택", choices, function(choice)
		local selected = browserMetaByName(choice.text)
		if not selected then
			hs.alert.show("BrowserRouter: 브라우저 선택값 해석 실패")
			return
		end
		onPick(selected)
	end)
end

function obj:_chooseProfile(browser, onPick)
	local ok, choicesOrErr = pcall(profileChoicesForBrowser, browser)
	local choices
	if ok then
		choices = choicesOrErr
	else
		print("BrowserRouter profileChoices error: " .. tostring(choicesOrErr))
		choices = nil
	end
	if type(choices) ~= "table" or #choices == 0 then
		choices = {
			{ text = "프로파일 사용 안 함", subText = "프로파일 목록을 읽지 못해 기본값으로 진행" },
		}
	end
	self:_showChooser("프로파일 선택", choices, function(choice)
		local selectedText = tostring(choice.text or "")
		if selectedText == "프로파일 사용 안 함" then
			onPick("")
			return
		end
		onPick(selectedText)
	end)
end

local function normalizeConfig(raw)
	local out = {
		default = {
			browser = "Google Chrome",
			profile = "Default",
		},
		rules = {},
	}
	if type(raw) ~= "table" then
		return out
	end

	if type(raw.default) == "table" then
		if type(raw.default.browser) == "string" and raw.default.browser ~= "" then
			out.default.browser = raw.default.browser
		end
		if type(raw.default.profile) == "string" then
			out.default.profile = raw.default.profile
		end
	end

	if type(raw.rules) == "table" then
		for _, rule in ipairs(raw.rules) do
			if type(rule) == "table" then
				local url = tostring(rule.url or "")
				local browser = tostring(rule.browser or "")
				local profile = tostring(rule.profile or "")
				if url ~= "" and browser ~= "" then
					table.insert(out.rules, {
						url = url,
						browser = browser,
						profile = profile,
					})
				end
			end
		end
	end
	return out
end

local function saveConfig(self)
	local encoded = hs.json.encode(config, true)
	if not encoded then
		hs.alert.show("BrowserRouter: JSON 인코딩 실패")
		return false
	end
	local ok, err = writeFile(rulesPath, encoded)
	if not ok then
		hs.alert.show("BrowserRouter: 저장 실패")
		if err then
			print("BrowserRouter save error: " .. tostring(err))
		end
		return false
	end
	self:_refreshMenuTitle()
	return true
end

function obj:_loadConfig()
	if not fileExists(rulesPath) then
		config = normalizeConfig(nil)
		saveConfig(self)
		return
	end
	local body = readFile(rulesPath)
	if not body or body == "" then
		config = normalizeConfig(nil)
		saveConfig(self)
		return
	end

	local decoded = hs.json.decode(body)
	config = normalizeConfig(decoded)
end

function obj:_refreshMenuTitle()
	if not menuBar then
		return
	end
	local count = config and config.rules and #config.rules or 0
	menuBar:setTitle(obj.menuTitle .. "(" .. tostring(count) .. ")")
end

local function promptText(title, message, defaultText, okText)
	local button, text = hs.dialog.textPrompt(title, message, defaultText or "", okText or "확인", "취소")
	if button ~= (okText or "확인") and button ~= "확인" and button ~= "OK" and button ~= "Choose" then
		return nil
	end
	if not text then
		return nil
	end
	local trimmed = text:gsub("^%s+", ""):gsub("%s+$", "")
	if trimmed == "" then
		return nil
	end
	return trimmed
end

function obj:_promptAddRule()
	local url = promptText(
		"Browser Router",
		"URL 또는 도메인 입력\n예) github.com 또는 https://mail.google.com",
		"",
		"다음"
	)
	if not url then
		return
	end

	self:_chooseBrowser(function(browser)
		hs.timer.doAfter(0.15, function()
			self:_chooseProfile(browser, function(profile)
				table.insert(config.rules, {
					url = url,
					browser = browser.name,
					profile = profile,
				})
				if saveConfig(self) then
					hs.alert.show("BrowserRouter: 규칙 추가됨")
				end
			end)
		end)
	end)
end

function obj:_promptSetDefault()
	self:_chooseBrowser(function(browser)
		hs.timer.doAfter(0.15, function()
			self:_chooseProfile(browser, function(profile)
				config.default.browser = browser.name
				config.default.profile = profile
				if saveConfig(self) then
					hs.alert.show("BrowserRouter: 기본값 저장됨")
				end
			end)
		end)
	end)
end

function obj:_promptDeleteRule()
	local rules = config.rules or {}
	if #rules == 0 then
		hs.alert.show("BrowserRouter: 삭제할 규칙이 없습니다")
		return
	end

	local choices = {}
	for idx, rule in ipairs(rules) do
		table.insert(choices, {
			text = string.format("%d. %s", idx, rule.url),
			subText = string.format("%s (%s)", rule.browser, rule.profile ~= "" and rule.profile or "no profile"),
			index = idx,
		})
	end

	self:_showChooser("삭제할 규칙 선택", choices, function(choice)
		local index = tonumber(choice.index)
		if not index or not config.rules[index] then
			hs.alert.show("BrowserRouter: 규칙 선택 오류")
			return
		end

		table.remove(config.rules, index)
		if saveConfig(self) then
			hs.alert.show("BrowserRouter: 규칙 삭제됨")
		end
	end)
end

local function isProfileCapableBrowser(browser)
	local meta = browserMetaByName(browser)
	if meta and meta.userDataDir then
		return true
	end
	local lower = string.lower(browser or "")
	return string.find(lower, "chrome", 1, true)
		or string.find(lower, "chromium", 1, true)
		or string.find(lower, "brave", 1, true)
		or string.find(lower, "edge", 1, true)
end

local function splitLabels(hostname)
	local labels = {}
	for label in string.gmatch(hostname or "", "[^.]+") do
		table.insert(labels, label)
	end
	return labels
end

local function parseURLParts(url)
	local scheme, hostPart, pathPart = string.match(url or "", "^(https?)://([^/?#]+)([^?#]*)")
	if not scheme then
		return nil
	end
	local host = string.lower((hostPart or ""):gsub(":%d+$", ""))
	local path = pathPart or ""
	if path == "" then
		path = "/"
	end
	return { scheme = string.lower(scheme), host = host, path = path }
end

local function parseRuleURL(ruleUrl)
	local raw = string.lower((ruleUrl or ""):gsub("^%s+", ""):gsub("%s+$", ""))
	if raw == "" then
		return nil
	end

	local parsed = parseURLParts(raw)
	if parsed then
		return {
			type = "url_prefix",
			value = raw,
			host = parsed.host,
			path = parsed.path,
		}
	end

	local hostPart, pathPart = string.match(raw, "^([^/]+)(/.*)$")
	if hostPart then
		return {
			type = "host_path",
			host = hostPart,
			path = pathPart,
		}
	end

	return {
		type = "host_only",
		host = raw,
	}
end

local function hostMatchType(ruleHost, targetHost)
	local normalizedRuleHost = string.lower((ruleHost or ""):gsub("^%.*", ""):gsub("%.*$", ""))
	local normalizedTargetHost = string.lower(targetHost or "")
	if normalizedRuleHost == "" or normalizedTargetHost == "" then
		return nil
	end

	local labels = splitLabels(normalizedRuleHost)
	local exactOnly = #labels > 2

	if normalizedTargetHost == normalizedRuleHost then
		if exactOnly then
			return "exact"
		end
		return "domain_exact"
	end

	if not exactOnly and normalizedTargetHost:sub(-(#normalizedRuleHost + 1)) == "." .. normalizedRuleHost then
		return "subdomain"
	end
	return nil
end

local function matchRule(ruleUrl, host, fullURL)
	local parsedRule = parseRuleURL(ruleUrl)
	if not parsedRule then
		return nil
	end

	local targetHost = string.lower(host or "")
	local targetURL = string.lower(fullURL or "")
	local targetParts = parseURLParts(targetURL)

	if parsedRule.type == "url_prefix" then
		if targetURL:sub(1, #parsedRule.value) == parsedRule.value then
			return 30000 + #parsedRule.value
		end
		return nil
	end

	local matchType = hostMatchType(parsedRule.host, targetHost)
	if not matchType then
		return nil
	end

	local hostLabels = #splitLabels(parsedRule.host)
	local baseScore
	if matchType == "exact" then
		baseScore = 22000
	elseif matchType == "domain_exact" then
		baseScore = 19000
	else
		baseScore = 15000
	end
	local score = baseScore + (hostLabels * 100)

	if parsedRule.type == "host_path" then
		local targetPath = targetParts and targetParts.path or "/"
		if targetPath:sub(1, #parsedRule.path) ~= parsedRule.path then
			return nil
		end
		score = score + #parsedRule.path
	end

	return score
end

local function pickTarget(host, fullURL)
	local bestRule
	local bestScore = -1
	for _, rule in ipairs(config.rules or {}) do
		local score = matchRule(rule.url, host, fullURL)
		if score and score > bestScore then
			bestScore = score
			bestRule = rule
		end
	end

	if bestRule then
		return {
			browser = bestRule.browser,
			profile = bestRule.profile,
		}
	end

	return {
		browser = config.default.browser,
		profile = config.default.profile,
	}
end

local function openURL(browser, profile, url)
	local meta = browserMetaByName(browser)
	if isProfileCapableBrowser(browser) and profile and profile ~= "" then
		local appPath = appBundlePathForBrowser(browser)
		local executablePath = appPath and meta and meta.executable and (appPath .. "/Contents/MacOS/" .. meta.executable) or nil
		local executableAttrs = executablePath and hs.fs.attributes(executablePath) or nil
		local commonArgs = {
			"--profile-directory=" .. profile,
			"--new-window",
			url,
		}
		if meta and meta.userDataDir and meta.userDataDir ~= "" then
			table.insert(commonArgs, 1, "--user-data-dir=" .. meta.userDataDir)
		end

		local debugPath = spoonDir and (spoonDir .. "/open_debug.log") or nil
		local debugLine = os.date("%Y-%m-%d %H:%M:%S")
			.. " browser=" .. tostring(browser)
			.. " profile=" .. tostring(profile)
			.. " url=" .. tostring(url)
			.. " exec=" .. tostring(executablePath)
			.. "\n"
		if debugPath then
			appendFile(debugPath, debugLine)
		end

		if executableAttrs and executableAttrs.mode == "file" then
			hs.task.new(executablePath, nil, commonArgs):start()
			return
		end

		hs.task
			.new("/usr/bin/open", nil, {
				"-na",
				browser,
				"--args",
				table.unpack(commonArgs),
			})
			:start()
		return
	end

	hs.task
		.new("/usr/bin/open", nil, {
			"-a",
			browser,
			url,
		})
		:start()
end

function obj:_buildMenu()
	local items = {
		{
			title = "Add Rule...",
			fn = function()
				self:_promptAddRule()
			end,
		},
		{
			title = "Set Default...",
			fn = function()
				self:_promptSetDefault()
			end,
		},
		{
			title = "Delete Rule...",
			fn = function()
				self:_promptDeleteRule()
			end,
		},
		{
			title = "Reload Rules",
			fn = function()
				self:_loadConfig()
				self:_refreshMenuTitle()
				hs.alert.show("BrowserRouter: rules.json 다시 읽음")
			end,
		},
		{
			title = "Open rules.json",
			fn = function()
				hs.task.new("/usr/bin/open", nil, { rulesPath }):start()
			end,
		},
		{ title = "-" },
	}

	for idx, rule in ipairs(config.rules or {}) do
		local label = string.format(
			"%d. %s -> %s (%s)",
			idx,
			rule.url,
			rule.browser,
			rule.profile ~= "" and rule.profile or "no profile"
		)
		table.insert(items, {
			title = label,
			disabled = true,
		})
	end

	if #(config.rules or {}) == 0 then
		table.insert(items, {
			title = "등록된 규칙 없음",
			disabled = true,
		})
	end

	return items
end

function obj:start()
	spoonDir = spoonDir or getSpoonDir()
	if not spoonDir then
		error("BrowserRouter: Spoon 경로 확인 실패")
	end
	local myBundleID = hs.processInfo.bundleID
	local function ensureDefaultHandler(scheme)
		local okGet, current = pcall(hs.urlevent.getDefaultHandler, scheme)
		if okGet and current == myBundleID then
			return
		end
		pcall(hs.urlevent.setDefaultHandler, scheme)
	end
	ensureDefaultHandler("http")
	ensureDefaultHandler("https")
	rulesPath = spoonDir .. "/rules.json"
	self:_loadConfig()

	if not menuBar then
		menuBar = hs.menubar.new()
	end
	menuBar:setMenu(function()
		return self:_buildMenu()
	end)
	self:_refreshMenuTitle()

	previousHttpCallback = hs.urlevent.httpCallback
	hs.urlevent.httpCallback = function(scheme, host, params, fullURL)
		local callbackDebugPath = spoonDir and (spoonDir .. "/open_debug.log") or nil
		if callbackDebugPath then
			appendFile(
				callbackDebugPath,
				os.date("%Y-%m-%d %H:%M:%S")
					.. " callback scheme="
					.. tostring(scheme)
					.. " host="
					.. tostring(host)
					.. " url="
					.. tostring(fullURL)
					.. "\n"
			)
		end
		if fullURL and fullURL ~= "" then
			local target = pickTarget(host, fullURL)
			openURL(target.browser, target.profile, fullURL)
			return
		end
		if previousHttpCallback then
			previousHttpCallback(scheme, host, params, fullURL)
		end
	end
end

function obj:stop()
	if menuBar then
		menuBar:delete()
		menuBar = nil
	end
	for chooser, _ in pairs(activeChoosers) do
		chooser:hide()
	end
	activeChoosers = {}
	if hs.urlevent.httpCallback then
		hs.urlevent.httpCallback = previousHttpCallback
	end
end

return obj
