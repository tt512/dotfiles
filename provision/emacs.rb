package 'emacs' do
  action :install
  user "root"
end

link File.expand_path('~/.emacs.d') do
  to File.expand_path('~/dotfiles/.emacs.d')
end

package 'skk-jisyo' do
  action :install
  user "root"
end

execute 'generate learge jisyo' do
  command 'skkdic-expr2 SKK-JISYO.L SKK-JISYO.propernoun SKK-JISYO.geo SKK-JISYO.jinmei > ~/SKK-JISYO.XL'
  cwd '/usr/share/skk'
end
