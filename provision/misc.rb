%w{firefox flashplugin}.each do |pkg|
  package pkg do
    action :install
    user "root"
  end
end

package "emacs" do
  action :install
  user "root"
end

link "#{ENV['HOME']}/.emacs.d" do
  action :create
  to "#{ENV['HOME']}/dotfiles/.emacs.d"
end

%w{zathura zathura-pdf-mupdf}.each do |pkg|
  package pkg
    action :install
    user "root"
  end
end

package "alsa-utils" do
  action :install
  user "root"
end

link "#{ENV['HOME']}/.gitconfig" do
  action :create
  to "#{ENV['HOME']}/dotfiles/.gitconfig"
end

---
- hosts: 127.0.0.1
  vars:
    - ansible_python_interpreter: /usr/bin/python2
  tasks:
    - name: Download keysnail.xpi
      get_url: dest=/tmp url=https://raw.githubusercontent.com/mooz/keysnail/master/keysnail.xpi
    - name: Setup .keysnail.js
      file: src=~/dotfiles/.keysnail.js dest=~/.keysnail.js state=link
    - name: Install keysnail to firefox
      command: firefox keysnail.xpi chdir=/tmp

