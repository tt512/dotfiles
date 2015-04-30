package 'gvim' do
  action :install
  user "root"
end

%w{.vim .vimrc .gvimrc}.each do |file|
  link ENV['HOME'] do
    action :create
    to "#{ENV['HOME']}/dotfiles/#{file}"
  end
end

