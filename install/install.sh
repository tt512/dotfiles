sudo pacman -Syu
#sudoedit /etc/vconsole.conf
sudo pacman -S xorg-server xorg-server-utils xorg-xinit mesa xf86-video-intel xf86-input-synaptics xterm xclip
sudo pacman -S awesome rxvt-unicode slim
ln -s dotfiles/.Xresources.d
ln -s dotfiles/.xinitrc
mkdir .config
cd .config
ln -s ~/dotfiles/config/awesome
sudo pacman -S gvim emacs
sudo pacman -S firefox flashplugin
sudo pacman -S ttf-liberation otf-ipafont
setxkbmap -model pc104 -layout us -variant ,dvorak -option ctrl:swapcaps
localectl set-x11-keymap us pc104 ,dvorak ctrl:swapcaps
ln -s dotfiles/.keysnail.js
packer -S ttf-ricty
sudo pacman -S fcitx-mozc
la/usr/share/X11/xorg.conf.d
la /usr/share/X11/xorg.conf.d
less /etc/X11/xorg.conf.d/50-synaptics.conf
cd /etc/X11/xorg.conf.d
sudo cp 50-synaptics.conf 50-synaptics.conf.orig
sudoedit 50-synaptics.conf
ln -s dotfiles/.gitconfig
git submodule init
git submodule update
