package "zsh" do
  user "root"
end

%w{.zsh .zshrc}.each do |file|
  link File.expand_path("~/#{file}") do
    to "#{ENV['HOME']}/dotfiles/#{file}"
  end
end

