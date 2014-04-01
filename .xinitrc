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

# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice

export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus
ibus-daemon -d -x
skype &
dropboxd &

exec $1
