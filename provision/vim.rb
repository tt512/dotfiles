package 'gvim' do
  action :install
  user "root"
end

%w{.vim .vimrc .gvimrc}.each do |file|
  link ENV['HOME'] do
    action :create
    to "dotfiles/#{file}"
    cwd ENV['HOME']
  end
end

package 'ctags' do
  action :install
  user "root"
end

