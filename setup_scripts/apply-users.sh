#!/bin/sh

pushd ~/.dotfiles

user=$(whoami)
nix build ".#homeManagerConfigurations.$user.activationPackage" --impure
./result/activate

popd
