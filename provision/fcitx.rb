case node['platform']
when 'freebsd'
  fcitx_packages = %w{fcitx-m17n ja-fcitx-mozc}
else
  fcitx_packages = %w{fcitx-im fcitx-configtool fcitx-mozc}
end

fcitx_packages.each do |pkg|
  package pkg do
    action :install
    user "root"
  end
end

