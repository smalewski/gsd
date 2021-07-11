#!/bin/sh

echo "************************************************************************"
echo "* INSTALLING DEPENDENCIES **********************************************"
echo "************************************************************************"

set -o errexit

# silence apt-get
DEBIAN_FRONTEND=noninteractive
export DEBIAN_FRONTEND

apt-get update

echo "Downloading stack"

# Create directory tmp files
mkdir /home/vagrant/tmp

# Download stack
wget -qO /home/vagrant/tmp/stack-install.sh https://get.haskellstack.org/
chmod u+x /home/vagrant/tmp/stack-install.sh

echo "Installing stack"

# Install Stack
/home/vagrant/tmp/stack-install.sh

echo "************************************************************************"
echo "* BUILDING GSD *********************************************************"
echo "************************************************************************"

su -c ". /home/vagrant/gsd/vagrant/build.sh" vagrant
