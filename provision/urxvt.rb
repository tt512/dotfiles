%w{rxvt-unicode urxvt-perls}.each do |pkg|
  package pkg do
    user "root"
  end
end
