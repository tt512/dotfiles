%w{uim-gtk ja-uim-anthy}.each do |pkg|
  package pkg do
    user "root"
  end
end

