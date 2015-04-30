%w{zsh zsh-completions}.each do |pkg|
  package pkg do
    user "root"
  end
end

%w{.zsh .zshrc}.each do |file|
  link File.expand_path("~/#{file}") do
    to "#{ENV['HOME']}/dotfiles/#{file}"
  end
end

