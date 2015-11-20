#!/usr/bin/env ruby
#
# Copyright (C) 2015 tat <tat@s10>
#
# Distributed under terms of the MIT license.
#


%w{virtualbox net-tools qt4}.each do |pkg|
  package pkg do
    action :install
    user "root"
  end
end

remote_file "/etc/modules-load.d/virtualbox.conf" do
  user "root"
end

execute "load kernel modules" do
  command "modprobe vboxdrv vboxnetadp vboxnetflt vboxpci"
  user "root"
end
