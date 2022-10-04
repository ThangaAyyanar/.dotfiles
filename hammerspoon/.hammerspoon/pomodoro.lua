
-- http://github.com/dbmrq/dotfiles/

-- Cherry tomato (a tiny Pomodoro)


-----------------------------
--  Customization Options  --
-----------------------------

-- Set these variables to whatever you prefer

-- The keyboard shortcut to start the timer is composed of the `super`
-- modifiers + the `hotkey` value.

local super = {"ctrl", "alt", "cmd"}
local hotkey = "C"

local duration = 15 -- timer duration in minutes

-- set this to true to always show the menu bar item
-- (making the keyboard shortcut superfluous):
local alwaysShow = false


-------------------------------------------------------------------
--  Don't mess with this part unless you know what you're doing  --
-------------------------------------------------------------------

-- Setup {{{1

local updateTimer, updateMenu, start, pause, reset, stop

local timestart, timestop

local menu
local isActive = false

local timeLeft = duration * 60

local timer = hs.timer.new(1, function() updateTimer() end)

local tink_sound = hs.sound.getByName("Tink")

-- }}}1

updateTimer = function()-- {{{1
    if not isActive then return end
    timeLeft = timeLeft - 1
    updateMenu()
    if timeLeft <= 0 then
        stop()
        hs.notify.new({title="Pomodoro Timer", informativeText="Done! 🍒"}):send()
        tink_sound:play()
        --hs.alert.show("Done! 🍒")
        timestop()
    end
end-- }}}1

updateMenu = function()-- {{{1
    if not menu then
        menu = hs.menubar.new()
        menu:setTooltip("Cherry")
    end
    menu:returnToMenuBar()
    local minutes = math.floor(timeLeft / 60)
    local seconds = timeLeft - (minutes * 60)
    local string = string.format("%02d:%02d 🍒", minutes, seconds)
    menu:setTitle(string)

    local items = {
            {title = "Stop", fn = function() stop() end},
        }
    if isActive then
        table.insert(items, 1, {title = "Pause", fn = function() pause() end})
    else
        table.insert(items, 1, {title = "Start", fn = function() start() end})
    end
    menu:setMenu(items)
end-- }}}1

start = function()-- {{{1
    if isActive then return end
    timer:start()
    timestart()
    isActive = true
end-- }}}1

pause = function()-- {{{1
    if not isActive then return end
    timer:stop()
    isActive = false
    updateMenu()
end-- }}}1

stop = function()-- {{{1
    pause()
    timestop()
    timeLeft = duration * 60
    if not alwaysShow then
        menu:delete()
    else
        updateMenu()
    end
end-- }}}1

reset = function()-- {{{1
    timeLeft = duration * 60
    updateMenu()
end-- }}}1

-- Time warrior with pomodoro

timestart = function()-- {{{1

    hs.notify.new({title="Time warrior", informativeText="Started"}):send()
    newSSID = hs.wifi.currentNetwork()
    if newSSID == workSSID then
        hs.execute("/usr/local/bin/timew start office")
    else
        hs.execute("/usr/local/bin/timew start personal")
    end
end-- }}}1

timestop = function() 
    hs.notify.new({title="Time warrior", informativeText="Done"}):send()
    hs.execute("/usr/local/bin/timew stop")
end

hs.hotkey.bind(super, hotkey, function() start() end)

if alwaysShow then updateMenu() end

