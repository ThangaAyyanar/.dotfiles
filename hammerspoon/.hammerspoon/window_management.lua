---- https://github.com/andweeb/.hammerspoon/blob/master/window-resizer.lua

local WindowResizer = {}
local mash_shift = {"ctrl", "alt", "shift"}

hs.hotkey.bind(mash_shift, 'H', function() hs.window.focusedWindow():focusWindowWest() end)
	hs.hotkey.bind(mash_shift, 'L', function() hs.window.focusedWindow():focusWindowEast() end)
	hs.hotkey.bind(mash_shift, 'K', function() hs.window.focusedWindow():focusWindowNorth() end)
	hs.hotkey.bind(mash_shift, 'J', function() hs.window.focusedWindow():focusWindowSouth() end)


hs.hotkey.bind({"cmd","alt","ctrl"}, "F", function()
    WindowResizer.makeWindowFullScreen()
end)

--- Move window to the left side
function WindowResizer.makeWindowFullScreen()

    local window = hs.window.focusedWindow()
    local screenDimensions = WindowResizer.getScreenDimensions()
    local windowFrame = window:frame()
    
    windowFrame.x = screenDimensions.x
    windowFrame.y = screenDimensions.y
    windowFrame.w = screenDimensions.w
    windowFrame.h = screenDimensions.h
    window:setFrame(windowFrame)
end

hs.hotkey.bind({"cmd","alt","ctrl"}, "H", function()
    WindowResizer.moveWindowLeft()
end)

--- Move window to the left side
function WindowResizer.moveWindowLeft()

    local window = hs.window.focusedWindow()
    local screenDimensions = WindowResizer.getScreenDimensions()
    local windowFrame = window:frame()
    
    windowFrame.x = screenDimensions.x
    windowFrame.y = screenDimensions.y
    windowFrame.w = screenDimensions.w / 2
    windowFrame.h = screenDimensions.h
    window:setFrame(windowFrame)
end

hs.hotkey.bind({"cmd","alt","ctrl"}, "L", function()
    WindowResizer.moveWindowRight()
end)

--- Move window to the right side
function WindowResizer.moveWindowRight()
    local window = hs.window.focusedWindow()
    local screenDimensions = WindowResizer.getScreenDimensions()
    local windowFrame = window:frame()
    
    windowFrame.x = screenDimensions.x + (screenDimensions.w / 2)
    windowFrame.y = screenDimensions.y
    windowFrame.w = screenDimensions.w / 2
    windowFrame.h = screenDimensions.h
    window:setFrame(windowFrame)
end

hs.hotkey.bind({"cmd","alt","ctrl"}, "U", function()
    WindowResizer.moveWindowUpperLeft()
end)

--- Move window to the upper left corner
function WindowResizer.moveWindowUpperLeft()
    
    local window = hs.window.focusedWindow()
    local windowFrame = window:frame()
    local screenDimensions = WindowResizer.getScreenDimensions()
    
    windowFrame.x = screenDimensions.x
    windowFrame.y = screenDimensions.y
    windowFrame.w = screenDimensions.w / 2
    windowFrame.h = (screenDimensions.h / 2)
    window:setFrame(windowFrame)
end

hs.hotkey.bind({"cmd","alt","ctrl"}, "I", function()
    WindowResizer.moveWindowUpperRight()
end)

function WindowResizer.moveWindowUpperRight()
    
    local window = hs.window.focusedWindow()
    local windowFrame = window:frame()
    local screenDimensions = WindowResizer.getScreenDimensions()
    
    windowFrame.x = screenDimensions.x + (screenDimensions.w / 2)
    windowFrame.y = screenDimensions.y
    windowFrame.w = screenDimensions.w / 2
    windowFrame.h = (screenDimensions.h / 2)
    window:setFrame(windowFrame)
end

hs.hotkey.bind({"cmd","alt","ctrl"}, "N", function()
    WindowResizer.moveWindowBottomLeft()
end)

--- Move window to bottom left
function WindowResizer.moveWindowBottomLeft()
    
    local window = hs.window.focusedWindow()
    local windowFrame = window:frame()
    local screenDimensions = WindowResizer.getScreenDimensions()
    
    windowFrame.x = screenDimensions.x
    windowFrame.y = screenDimensions.y + (screenDimensions.h / 2)
    windowFrame.w = screenDimensions.w / 2
    windowFrame.h = (screenDimensions.h / 2) +50
    window:setFrame(windowFrame)
end

hs.hotkey.bind({"cmd","alt","ctrl"}, "M", function()
    WindowResizer.moveWindowBottomRight()
end)

-- Move window to bottom right
function WindowResizer.moveWindowBottomRight()
    
    local window = hs.window.focusedWindow()
    local windowFrame = window:frame()
    local screenDimensions = WindowResizer.getScreenDimensions()
    
    windowFrame.x = screenDimensions.x + (screenDimensions.w / 2)
    windowFrame.y = screenDimensions.y + (screenDimensions.h / 2) 
    windowFrame.w = screenDimensions.w / 2
    windowFrame.h = (screenDimensions.h / 2)
    window:setFrame(windowFrame)
end

--- Move window to center
function WindowResizer.centerWindow()

    local window = hs.window.focusedWindow()
    local windowFrame = window:frame()
    local screenDimensions = WindowResizer.getScreenDimensions()
    
    windowFrame.x = screenDimensions.x + (screenDimensions.w * 0.25)
    windowFrame.y = screenDimensions.y + (screenDimensions.h * 0.25)
    windowFrame.w = screenDimensions.w * 0.5
    windowFrame.h = screenDimensions.h * 0.5
    window:setFrame(windowFrame)
end

--hs.hotkey.bind({"cmd","alt","ctrl"}, "F", function()
    --WindowResizer.moveWindowBottomRight()
--end)

--- Toggle Window fullscreen
function WindowResizer.fullScreenWindow()
    local window = hs.window.focusedWindow()
    window:toggleFullScreen()
end

--- Get Screen dimensions utility function
function WindowResizer.getScreenDimensions()
    return hs.window.focusedWindow():screen():frame()
end
