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
  to "dotfiles/config/awesome"
  cwd ENV['HOME']
end
