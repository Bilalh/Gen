<style type="text/css">
img {
  text-align: center;
  max-width: 400px;
  display: block;
}
</style>


password:instancegen1#
![setup](Screen%20Shot%202014-02-10%20at%2020.18.52.png)
takes at lest 10 minutes to create a vm

download cmd line tools from
http://www.windowsazure.com/en-us/downloads/?fb=en-us

notes
http://www.windowsazure.com/en-us/documentation/articles/xplat-cli/

ssh details
http://www.windowsazure.com/en-us/documentation/articles/virtual-machines-linux-how-to-log-on/

https://manage.windowsazure.com

ssh into vm

	ssh azureuser@instancegen1.cloudapp.net
	azureuser@instancegen1.cloudapp.net's password:instancegen1#

	azureuser@instancegen:~$ pwd
	/home/azureuser


Using 12.04 LTS firstly

	sudo locale-gen en_GB.UTF-8


tmux is allready installed:

	cat > .tmux.conf <<-EOF
	# remap prefix to Control + a
	set -g prefix C-a
	unbind C-b
	bind C-a send-prefix
	EOF
	tmux source-file ~/.tmux.conf


	tmux new -s bilal
	#tmux attach -t bilal

install everything things

	sudo apt-get install python-software-properties
	sudo add-apt-repository ppa:fkrull/deadsnakes

	sudo apt-get update

	sudo apt-get install build-essential libssl-dev libreadline-dev libsqlite3-dev   make  ruby libgmp3c2 g++ cmake  git mercurial htop screen  zsh subversion  llvm clang  sqlite3  pigz r-base r-base-dev  openjdk-7-jdk openjdk-7-jre  libgmp3-dev binutils python3.3  python3.3-dev libboost-all-dev python-pip

	# installing pip is silly
	wget https://bitbucket.org/pypa/setuptools/raw/bootstrap/ez_setup.py -O /tmp/ez_setup.py
	sudo python3.3 /tmp/ez_setup.py
	sudo easy_install-3.3 pip
	sudo pip3.3 install docopt pathlib


	# installing parallel
	mkdir ~/tmp ; cd ~/tmp
	wget http://ftp.heanet.ie/mirrors/gnu/parallel/parallel-latest.tar.bz2
	tar xvjf parallel-latest.tar.bz2
	cd parallel-*
	sudo ./configure && sudo make && sudo make install
	cd ; rm -rf ~/tmp


	sys info
	System load:  0.0                Processes:           106
	Usage of /:   11.6% of 28.83GB   Users logged in:     0
	Memory usage: 4%                 IP address for eth0: 100.88.150.62
	Swap usage:   0%


install repos


	mkdir ~/repos
	cd ~/repos
	hg clone https://Bilalh@bitbucket.org/stacs_cp/conjure
	cd conjure
	time make

	cd ~/repos/
	git clone ssh://bh246@keith.cs.st-andrews.ac.uk/mnt/raid/repositories/minion
	cd minion
	mkdir build
	cd build
	cmake ..
	time make -j4 minion
	# 13m/ 46m cpu

	cd ~/repos/
	cd savilerow
	./complie.sh

	cd ~/repos/
	git clone https://Bilalh@bitbucket.org/Bilalh/instancegen-models.git
	git clone https://Bilalh@bitbucket.org/Bilalh/instancegen.git

	# setup path
	cat > ~/.bash_profile <<-EOF
	PATH=\$PATH:\$HOME/server-settings/bin
	PATH=\$PATH:\$HOME/bin
	PATH=\$PATH:\$HOME/repos/minion/build
	PATH=\$PATH:\$HOME/repos/savilerow
	PATH=\$PATH:\$HOME/.cabal/bin
	PATH=\$PATH:\$HOME/repos/conjure/dist/tools/ghc-7.6.3-build/bin
	export PATH

	if [ -f ~/.bashrc ]; then
	        source ~/.bashrc
	fi
	EOF


python3.3 on 12.04  is called python3.3 not python3

	cd
	mkdir bin
	cd bin
	ln -s /usr/bin/python3.3 python3


setting up ssh keys

	ssh-keygen -t rsa -C "bh246@st-andrews.ac.uk"
	#Enter file in which to save the key
	/Users/bilalh/.ssh/id_azure_rsa

	# assuming ssh-copy-id is installed on the local machine
	ssh-copy-id -i id_azure_rsa azureuser@instancegen1.cloudapp.net

add this to ~/.ssh/config

	Host ig1
		user azureuser
		Hostname  instancegen1.cloudapp.net
		PreferredAuthentications publickey
		Identityfile ~/.ssh/id_azure_rsa


command

	essence=../../instancegen-models/prob013-PPP/prob013-PPP.essence ; dirr="__/__uniform/"; ./uniform_sampling.py iterations 1 --output_dir=$dirr --essence=$essence --working_dir="`dirname $essence`" --info="`dirname $essence`/info.json" --mode='df-no-channelling-better' --models_timeout=100 --seed 517917

compress results to make it easier to rsync

	cd ~/repos/instancegen/mchain/__
	export MODE='df-no-channelling-better'
	$PARAM_GEN_SCRIPTS/misc/tar_results.sh __uniform/

Getting results from azure

	rsync -av --compress ig1:repos/instancegen/mchain/__/__uniform/  __uniform


When logging in again

	#9 packages can be updated.
	#7 updates are security updates
	# so I ran
	sudo apt-get upgrade



To save the vm to a  vhd image, so that it can be reused

** THIS WILL DELETE THE VM **

run this command to prepare the vm for reuse

	sudo waagent -deprovision

	WARNING! The waagent service will be stopped.
	WARNING! All SSH host key pairs will be deleted.
	WARNING! Nameserver configuration in /etc/resolvconf/resolv.conf.d/{tail,originial} will be deleted.
	WARNING! Cached DHCP leases will be deleted.
	Do you want to proceed (y/n)? y

Click `SHUT DOWN`
Click `CAPTURE`

![screen](Screen%20Shot%202014-02-15%20at%2004.11.02.png)


Make a vm from a vhd

Create the New button 

	Compute -> Virtual machine -> from gallery 
	
then
 
	my images  -> instancegen1v2


if recreating the vhd to the some host  then use have the remove the rsa key from `know_hosts` or you will get
a `WARNING: REMOTE HOST IDENTIFICATION HAS CHANGED!` error when using ssh

