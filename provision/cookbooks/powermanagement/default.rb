#!/usr/bin/env ruby

remote_file "/etc/udev/rules.d/99-lowbat.rules" do
  user "root"
  owner "root"
  group "root"
end


