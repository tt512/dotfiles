#!/usr/bin/env ruby
#
# default.rb
# Copyright (C) 2015 tat <tat@s10>
#
# Distributed under terms of the MIT license.
#


%w{iw wpa_supplicant}.each do |pkg|
  package pkg do
    user "root"
  end
end

remote_file "/etc/systemd/system/network-wireless@.service" do
  user "root"
  owner "root"
  group "root"
end

service "network-wireless@wlp10s0.service" do
  user "root"
  action [:start, :enable]
end

