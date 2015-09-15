#case node['platform']
#when 'freebsd'
#  xorg_packages = %w{xorg xf86-input-synaptics xclip}
#else
#  xorg_packages = %w{xorg-server xorg-server-utils xorg-xinit mesa xf86-video-intel xf86-input-synaptics xclip}
#end
#xorg_packages.each do |pkg|

%w{xorg-server xorg-server-utils xorg-xinit mesa xf86-video-intel xf86-input-synaptics xclip xorg-xev}.each do |pkg|
  package pkg do
    action :install
    user "root"
  end
end

%w{.xinitrc .Xresources.d .Xmodmap}.each do |file|
  link "#{ENV['HOME']}/#{file}" do
    action :create
    to "#{ENV['HOME']}/dotfiles/#{file}"
  end
end

remote_file "/etc/X11/xorg.conf.d/50-synaptics.conf" do
  owner "root"
  group "root"
  user "root"
end

remote_file "/etc/X11/xorg.conf.d/10-keyboard.conf" do
  owner "root"
  group "root"
  user "root"
end

%w{rxvt-unicode urxvt-perls}.each do |pkg|
  package pkg do
    action :install
    user "root"
  end
end

# autologin to tty1
remote_file "/etc/systemd/system/getty@tty1.service.d/override.conf" do
  user "root"
  owner "root"
  group "root"
  mode "0644"
end

remote_file File.expand_path("~/.zlogin")
