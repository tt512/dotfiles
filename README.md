My dotfiles
===========

Installation
------------

```
sudo pacman -S ansible
echo '127.0.0.1' > ~/ansible_hosts
export ANSIBLE_HOSTS=~/ansible_hosts
cd ~/dotfiles/install
ansible-playbook archlinux-letsnote.yml -c local -K
```
