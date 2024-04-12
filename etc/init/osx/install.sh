#!/bin/bash

set -u

# 実行場所のディレクトリを取得
THIS_DIR=$(cd $(dirname $0); pwd)

cd $THIS_DIR
git submodule init
git submodule update


# emacs set up
if which cask >/dev/null 2>&1; then
  echo "setup .emacs.d..."
  cd ${THIS_DIR}/.emacs.d
  cask upgrade
  cask install
fi

cat << END

**************************************************
DOTFILES SETUP FINISHED! bye.
**************************************************

END
