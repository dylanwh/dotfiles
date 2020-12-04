function proxycmd --argument host cmd
echo -e "#!/bin/sh\nssh $host $cmd" | sudo tee /usr/local/bin/$cmd > /dev/null
sudo chmod 755 /usr/local/bin/$cmd
end
