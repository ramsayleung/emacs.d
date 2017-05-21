#!/usr/bin/env bash
#Description:
#resolve the dependency for emacs(to make it become a delightful os)
set -e
set -x

# check if pip exists
type pip>/dev/null 2>&1 || {
    echo >&2 " require pip but it's not installed.  Aborting.";
    exit 1;
}
# check if npm exists 
type npm>/dev/null 2>&1 || {
    echo >&2 " require pip but it's not installed.  Aborting.";
    exit 1;
}
#run as root
echo "start to install emacs  dependecy"
#emacs javascript-edit mode (ie js2-mode depedency):
#install "tern" to use the auto-completion and documentation features
echo "Start to install tern"
sudo npm install -g tern
#to use the formatting features,install js-beautify
echo "Start to install js-beautify"
sudo npm install -g js-beautify
#syntax checker Program;which Flycheck dependens on
#  install Pylint for Python
echo "start to install Python syntax checker"
sudo pip install pylint
# install Eslint for Javascript
echo "start to install Javascript synatx checker"
sudo npm install eslint -g
#install required Python packages for Elpy
#if [ $(pip install rope;echo $?) != 0 ]
#Either of these
echo "Start to install rope "
sudo pip install rope
#pip install jedi
#Flake8 for code checks;syntax checker as pylint
echo "Start to install flake8"
sudo pip install flake8
#importmagic for aotumatic imports as IDE does
echo  "Start to install importmagic"
sudo pip install importmagic
#To be able to suppress unused imports easily,install autoflake
echo "Start to install autoflake"
sudo pip install autoflake
#and  autopep8 for automatic PEP8 formatting(which is used to format python code)
echo "Start to install autopep8"
sudo pip install autopep8
#and yapf for code formatting
echo "Start to install yapf"
sudo pip install yapf 
#install virtualenv for jedi (python auto-completion)
echo "Start to install virtualenv"
sudo pip install virtualenv
#install jedi(anaconda dependencies)
echo "Start to install jedi"
sudo pip install jedi
#install json-rpc(anaconda dependencies)
echo "Start to install json-rpc"
sudo pip install json-rpc
#install service_factory(anaconda dependencies)
echo "Start to install service_factory"
sudo pip install service_factory
#install epc
echo "Start to install epc"
sudo pip install epc
# install isort
echo "Start to install isort"
sudo pip install isort

echo "Install gocode"
go get -u github.com/nsf/gocode

echo "Install rust-lang"
curl https://sh.rustup.rs -sSf | sh

echo "Install racer dependency"
rustup component add rust-src
cargo install racer
