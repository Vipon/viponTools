#!/bin/bash

DIR="$(dirname "$0")"
VIM_DIR="${HOME}/.vim"
VIM_AUTOLOAD_DIR="${VIM_DIR}/autoload"
VIM_COLOR_DIR="${VIM_DIR}/colors"
VIM_FILE_TYPE_DETECT_DIR="${VIM_DIR}/ftdetect"

installLatestVim()
{
    git clone https://github.com/vim/vim
    cd vim/src & ./configure --with-features=huge \
        --enable-gui=gnome2 \
        --prefix=$HOME/.local
    cd vim/src & make -j8
    cd vim/src & make install
}

installVimPlug()
{
    mkdir -p "${VIM_AUTOLOAD_DIR}"
    curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
}

installVimRc()
{
    cp "${DIR}/.vimrc" ~/
    mkdir -p "${VIM_FILE_TYPE_DETECT_DIR}"
    cp "${DIR}/kssLanguage.vim" "${VIM_FILE_TYPE_DETECT_DIR}"
}

installPlugins()
{
    vim +PlugInstall +q +q
}

installCocConfig()
{
    echo ${DIR}
    cp ${DIR}/coc-settings.json ~/.vim
}

installLanguageServers()
{
    pip3 install --user pyls
    pip3 install --user cmake-language-server
    sudo snap install bash-language-server
    sudo apt install clangd-9
    sudo apt install hoogle
}

main()
{
    installLatestVim
    installVimPlug
    installVimRc
    installPlugins
    installCocConfig
    installLanguageServers
}

main "$@"
