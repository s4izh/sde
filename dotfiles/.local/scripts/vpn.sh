firefox https://myupclink.upc.edu:443/remote/saml/start &
read VPN_COOKIE
sudo openfortivpn myupclink.upc.edu:443 --cookie="$VPN_COOKIE"
