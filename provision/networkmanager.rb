%w{networkmanager network-manager-applet gnome-keyring}.each do |pkg|
  package pkg do
    action :install
    user "root"
  end
end

service "NetworkManager" do
  action [:enable]
  user "root"
end
