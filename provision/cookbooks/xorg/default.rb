%w{xorg-server xorg-server-utils xorg-xinit mesa xf86-video-intel xf86-input-synaptics xclip rxvt-unicode urxvt-perls}.each do |pkg|
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


# awesome
package "awesome" do
  action :install
  user "root"
end

directory "#{ENV['HOME']}/.config" do
  action :create
end

link "#{ENV['HOME']}/.config/awesome" do
  action :create
  to "#{ENV['HOME']}/dotfiles/config/awesome"
end



%w{fcitx-im fcitx-configtool fcitx-mozc}.each do |pkg|
  package pkg do
    action :install
    user "root"
  end
end


%w{ttf-dejavu ttf-liberation otf-ipafont adobe-source-code-pro-fonts adobe-source-han-sans-jp-fonts}.each do |pkg|
  package pkg do
    action :install
    user "root"
  end
end

