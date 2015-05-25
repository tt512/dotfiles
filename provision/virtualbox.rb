#!/usr/bin/env ruby
#
# virtualbox.rb
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

execute "add user to vboxusers group" do
  command "gpasswd -a tat vboxusers"
end


