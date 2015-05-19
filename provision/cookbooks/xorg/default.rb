case node['platform']
when 'freebsd'
  xorg_packages = %w{xorg xf86-input-synaptics xclip}
else
  xorg_packages = %w{xorg-server xorg-server-utils xorg-xinit mesa xf86-video-intel xf86-input-synaptics xclip}
end
xorg_packages.each do |pkg|
  package pkg do
    action :install
    user "root"
  end
end

%w{.xinitrc .Xresources.d}.each do |file|
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

%w{rxvt-unicode urxvt-perls}.each do |pkg|
  package pkg do
    action :install
    user "root"
  end
end

