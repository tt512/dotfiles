%w{firefox flashplugin}.each do |pkg|
  package pkg do
    action :install
    user "root"
  end
end

#      get_url: dest=/tmp url=https://raw.githubusercontent.com/mooz/keysnail/master/keysnail.xpi
#
link "#{ENV['HOME']}/.keysnail.js" do
  to "#{ENV['HOME']}/dotfiles/.keysnail.js"
end

