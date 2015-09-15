
%w{cups ghostscript}.each do |pkg|
  package pkg do
    user "root"
  end
end

service "org.cups.cupsd" do
  action [:start, :enable]
  user "root"
end

%w{system-config-printer cups-pk-helper}.each do |pkg|
  package pkg do
    user "root"
  end
end

remote_file "/etc/polkit-1/rules.d/70-cupspkhelper.rules" do
  owner "root"
  group "root"
  mode "644"
  user "root"
end

