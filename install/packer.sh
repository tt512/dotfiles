cd /tmp
sudo pacman -S jshon expac
curl -O https://aur.archlinux.org/packages/pa/packer/PKGBUILD
makepkg
sudo pacman -U packer-*.pkg.tar.xz
