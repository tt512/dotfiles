case node['platform']
when "freebsd"
  font_packages = %w{dejavu liberation-fonts-ttf ja-font-takao sourcecodepro-ttf linuxlibertine}
else
  font_packages = %w{ttf-dejavu ttf-liberation otf-ipafont adobe-source-code-pro-fonts adobe-source-han-sans-jp-fonts}
end

font_packages.each do |pkg|
  package pkg do
    action :install
    user "root"
  end
end

