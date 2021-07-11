#!/bin/sh

cd gsd

export PATH=$PATH:/home/vagrant/.local/bin

stack install

echo "Done"
