#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

#[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
# From tagae/dotfiles
for resource in ~/.Xresources ~/.Xresources.d/* ; do
  [ -r $resource ] && xrdb -merge $resource
done

#[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap
#xmodmap ~/.Xmodmap

#setxkbmap -option ctrl:swapcaps
#xmodmap -e "keycode 102 = Super_L"

export GTK_IM_MODULE=fcitx
export XMODIFIERS="@im=fcitx"
export QT_IM_MODULE=fcitx
fcitx

skype &
dropboxd &

# exec gnome-session
# exec startkde
# ...or the Window Manager of your choice

exec awesome
#exec xmonad
#exec $1
