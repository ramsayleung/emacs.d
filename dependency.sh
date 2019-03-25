#!/usr/bin/env bash
#Description:
#resolve the dependency for emacs(to make it become a delightful os)
# set -e
# set -x
install_javascript_dep(){
    if command -v "npm">/dev/null;
    then
	# Use taobao mirror
	npm list cnpm -g || sudo npm install -g cnpm --registry=https://registry.npm.taobao.org
	echo "npm exists"
	#run as root
	echo "start to install emacs  dependecy"
	#to use the formatting features,install js-beautify if doesn't exist
	echo "Start to install js-beautify"
	cnpm list -g js-beautify || sudo cnpm i js-beautify -g
	#syntax checker Program;which Flycheck dependens on
	# install Eslint for Javascript
	echo "start to install Javascript synatx checker"
	cnpm list -g eslint || sudo cnpm install eslint -g
	#install required Python packages for Elpy
	#if [ $(pip install rope;echo $?) != 0 ]
    fi
}
install_python_dep(){
    if command -v pip3 >/dev/null;
    then
	echo "pip3 exists"
	# use pypi mirror
	pip install pip -U
	pip config set global.index-url https://pypi.tuna.tsinghua.edu.cn/simple
	#Either of these
	echo "Start to install language server"
	pip3  install 'python-language-server[all]' --user
    fi
}

install_go_dep(){
    # if go is installed, install go dependency
    if command -v "go">/dev/null;
    then
	echo "go exists"
	echo "Install goimports"
	go get -u -v golang.org/x/tools/cmd/goimports
	echo "Install gocode"
	go get -u -v github.com/mdempsky/gocode
	echo "Install godef"
	go get -u -v github.com/rogpeppe/godef
	echo "Install gogetdoc"
	go get -u -v github.com/zmb3/gogetdoc
	echo "Install golint"
	go get -u -v github.com/golang/lint/golint
	echo "Install go-outline"
	go get -u -v github.com/lukehoban/go-outline
	echo "Install goreturns"
	go get -u -v sourcegraph.com/sqs/goreturns
	echo "Install gorename"
	go get -u -v golang.org/x/tools/cmd/gorename
	echo "Install gogkgs"
	go get -u -v github.com/tpng/gopkgs
	echo "Install go-symbols"
	go get -u -v github.com/newhook/go-symbols
	echo "Install guru"
	go get -u -v golang.org/x/tools/cmd/guru
	echo "Install gotests"
	go get -u -v github.com/cweill/gotests
	echo "Install godoc"
	go get -u -v golang.org/x/tools/cmd/godoc
	echo "Install go-langserver"
	go get -u github.com/sourcegraph/go-langserver
	echo "Install gometalinter"
	curl -L https://git.io/vp6lP | sh
	echo "Install all available checkers"
	gometalinter --install --update
	echo "Install errcheck"
	go get -u -v github.com/kisielk/errcheck
    fi
}

install_rust_dep(){
    # rust isn't installed 
    if ! command -v "rustc" > /dev/null;
    then
	echo "Could not find rust, install rust-lang"
	curl https://sh.rustup.rs -sSf | sh
    fi
    echo "Install racer dependency"
    rustup component add rls-preview rust-analysis rust-src 
    # Throw an error if binary exists
    cargo install racer 
    cargo install cargo-edit 
    cargo install cargo-script 
}

main(){
    install_javascript_dep
    install_python_dep
    install_rust_dep
    # install_go_dep
}
main
