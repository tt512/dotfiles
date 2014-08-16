My dotfiles
===========

Installation
------------

```
% sudo pacman -S ansible
% echo '127.0.0.1' > ~/ansible_hosts
% export ANSIBLE_HOSTS=~/ansible_hosts
% ansible-playbook ~/dotfiles/install/archlinux-letsnote.yml --connection=local --ask-sudo-pass -e ansible_python_interpreter=/usr/bin/python2
```
