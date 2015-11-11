package 'git' do
  action :install
  user "root"
end

%w{.gitconfig .gitignore_global}.each do |file|
  link ENV['HOME'] do
    action :create
    to "#{ENV['HOME']}/dotfiles/#{file}"
  end
end
