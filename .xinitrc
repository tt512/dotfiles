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

[ -f ~/.xprofile ] && . ~/.xprofile

#[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
# From tagae/dotfiles
for resource in ~/.Xresources ~/.Xresources.d/*.xresources ; do
  [ -r $resource ] && xrdb -merge $resource
done

[ -f ~/.Xmodmap ] && xmodmap ~/.Xmodmap

# exec gnome-session
# exec startkde
# ...or the Window Manager of your choice

exec awesome
#exec xmonad
#exec $1
