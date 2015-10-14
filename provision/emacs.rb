package 'emacs' do
  action :install
  user "root"
end

link File.expand_path('~/.emacs.d') do
  to 'dotfiles/.emacs.d'
end

package 'skktools' do
  action :install
  user "root"
end

package 'skk-jisyo' do
  action :install
  user "root"
  notifies :run, 'execute[generate jisyo]'
end

execute 'generate jisyo' do
  action :nothing
  command 'skkdic-expr2 SKK-JISYO.L SKK-JISYO.propernoun SKK-JISYO.geo SKK-JISYO.jinmei > ~/SKK-JISYO.XL'
  cwd '/usr/share/skk'
end
