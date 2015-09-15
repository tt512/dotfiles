#!/usr/bin/env ruby

package "virtualbox-host-dkms" do
  user "root"
end

execute "make module" do
  command `dkms install vboxhost/$(pacman -Q virtualbox|awk {'print $2'}|sed 's/\-.\+//') -k $(uname -rm|sed 's/\ /\//')`
  user "root"
end

service "dkms" do
  action :enable
  user "root"
end

