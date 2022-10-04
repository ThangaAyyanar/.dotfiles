
require "pomodoro"
require "reload"
local WindowResizer = require("window_management")

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "W", function()
  hs.notify.new({title="Hammerspoon", informativeText="Hello World"}):send()
end)


hs.hotkey.bind({"cmd", "shift" }, "space", function()
  hs.execute('launch_alacritty',true)
end)

-- Url bind for taskwarrior
-- local taskwarriorOfficeMenuBar = hs.menubar.new()
-- local taskwarriorPersonalMenuBar = hs.menubar.new()
-- Below code doesn't work properly as it only stop timew not task
--taskwarriorOfficeMenuBar:setMenu({{title= "Stop project",fn = function() hs.execute([["/usr/local/bin/timew" "stop"]]) end }})
--taskWarriorBooksMenuBar = hs.menubar.new()

-- function setOfficeMenu(project)
--   if project then
--     taskwarriorOfficeMenuBar:returnToMenuBar():setTitle("Office"..' - '..project)
--   else
--     taskwarriorOfficeMenuBar:returnToMenuBar():setTitle("Office")
--   end
-- end

-- function setPersonalMenu(project)
--   if project then
--     taskwarriorPersonalMenuBar:returnToMenuBar():setTitle("Personal"..' - '..project)
--   else
--     taskwarriorPersonalMenuBar:returnToMenuBar():setTitle("Personal")
--   end
-- end

-- hs.urlevent.bind("task", function(eventName, params)
--     --hs.alert.show("Received someAlert")
--     if params["office"] then
--       office = params["office"]
--       if office == "1" then
--         setOfficeMenu(params["project"])
--       else
--         taskwarriorOfficeMenuBar:removeFromMenuBar()
--       end
--     elseif params["personal"] then
--       personal = params["personal"]
--       if personal == "1" then
--         setPersonalMenu(params["project"])
--       else
--         taskwarriorPersonalMenuBar:removeFromMenuBar()
--       end
--     end
-- end)
