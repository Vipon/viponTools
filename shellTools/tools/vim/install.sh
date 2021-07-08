#!/bin/bash
#
# MIT License
#
# Copyright (c) 2021 Konychev Valera
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

SCRIPT_DIR="$(realpath $(dirname "${BASH_SOURCE[0]}"))"
source "${SCRIPT_DIR}/../../os/libos.sh"

ROOT="$(realpath $(dirname "${BASH_SOURCE[0]}"))"
BUILD_DIR="${ROOT}/vim/src"
INSTALL_DIR="${HOME}/.local"
VIM_DIR="${HOME}/.vim"
VIM_AUTOLOAD_DIR="${VIM_DIR}/autoload"
VIM_COLOR_DIR="${VIM_DIR}/colors"
VIM_FILE_TYPE_DETECT_DIR="${VIM_DIR}/ftdetect"

installLatestVim()
{
    git clone https://github.com/vim/vim
    cd "${BUILD_DIR}"
    ./configure --with-features=huge    \
        --enable-gui=gnome2             \
        --prefix="${INSTALL_DIR}"

    make -j8
    make install
}

installVimPlug()
{
    mkdir -p "${VIM_AUTOLOAD_DIR}"
    curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
}

installVimRc()
{
    cp "${ROOT}/.vimrc" ~/
    mkdir -p "${VIM_FILE_TYPE_DETECT_DIR}"
    cp "${ROOT}/kssLanguage.vim" "${VIM_FILE_TYPE_DETECT_DIR}"
}

installPlugins()
{
    vim +PlugInstall +q +q
}

installCocConfig()
{
    echo ${ROOT}
    cp ${ROOT}/coc-settings.json ~/.vim
}

installLanguageServers()
{
    pip3 install --user pyls
    pip3 install --user cmake-language-server
    execIfLinux sudo snap install bash-language-server
    execIfMacOsX npm install bash-language-server
    sudo apt install clangd-9
    sudo apt install hoogle
}

main()
{
    cd "${ROOT}"
    execIfLinux installLatestVim
    installVimPlug
    installVimRc
    installPlugins
    installCocConfig
    installLanguageServers
}

main "$@"

