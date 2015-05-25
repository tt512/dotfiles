#!/usr/bin/env ruby
#
# keysnail.rb
# Copyright (C) 2015 tat <tat@s10>
#
# Distributed under terms of the MIT license.
#

aur_package "keysnail-git" do
  action :install
  user "root"
end

link File.expand_path("~/.keysnail.js") do
  to File.expand_path("~/dotfiles/.keysnail.js")
end

