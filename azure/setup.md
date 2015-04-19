#How to setup ssh with  Azure

The current version only accepts SSH public keys that are encapsulated in an X509 certificate. Follow the steps below to generate and use SSH keys with Azure.

## Generate Azure Compatible Keys ##


1. Use `openssl` to generate an X509 certificate with a 2048-bit RSA keypair. Please answer the few questions that the `openssl` prompts for (or you may leave them blank). The content in these fields is not used by the platform:
		# cd ~/.ssh
		# openssl req -x509 -nodes -days 365 -newkey rsa:2048 -keyout myPrivateKey.key -out myCert.pem

2.	Change the permissions on the private key to secure it.

		# chmod 600 myPrivateKey.key

3.	Upload the `myCert.pem` while creating the Linux virtual machine. The provisioning process will automatically install the public key in this certificate into the `authorized_keys` file for the specified user in the virtual machine.

4.	If you are going to use the API directly, and not use the Management Portal, convert the `myCert.pem` to `myCert.cer` (DER encoded X509 certificate) using the following command:

		# openssl  x509 -outform der -in myCert.pem -out myCert.cer
		

5.  To ssh
	
		ssh -i ~/.ssh/myPrivateKey.key <name>.cloudapp.net

# Install stuff 

Using 14.04 LTS firstly  to get rid of error message.

	sudo locale-gen en_GB.UTF-8


Install all dependencies for our stuff.

	sudo apt-get install build-essential libssl-dev libreadline-dev libsqlite3-dev   make  ruby g++ cmake  git mercurial htop screen  zsh subversion  llvm clang  sqlite3  pigz r-base r-base-dev  openjdk-7-jdk openjdk-7-jre  libgmp3-dev binutils  libboost-all-dev python-pip


## Setup a ssh key on azure

	cd ~/.ssh
	ssh-keygen 
	cat id_rsa.pub
	
copy the output to https://bitbucket.org/account/user/<USER>/ssh-keys/


## Settings

	cd
	git clone git@bitbucket.org:Bilalh/server-settings.git
	cd server-settings 
	mv ~/.bashrc ~/.bashrc.local
	./link_settings.sh
	
Install parallel 
	
	./install_parallel.sh


Install Repos

	cd
	./server-settings/install_repos.sh

