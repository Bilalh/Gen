#!/bin/bash
set -o nounset
echo "Installing python3.4 in $HOME/bin"

mkdir -p "$HOME/bin"
wget https://www.python.org/ftp/python/3.4.2/Python-3.4.2.tar.xz
tar xJf ./Python-3.4.2.tar.xz
cd ./Python-3.4.2
./configure --prefix="$HOME/bin/python3.4"
make && make install

ln -s "$HOME/bin/python3.4/bin/python3.4" "$HOME/bin/python3"

echo "Finished, installed as  $HOME/bin/python3"
echo "Make sure $HOME/bin is on your PATH"