#!/bin/bash


cat << START

**************************************************
THIS IS DOTFILES SETUP START SCRIPT
**************************************************

START


OS="null"

SCRIPT_DIR="$(cd $(dirname $0); pwd)"
echo "SCRIPT_DIR=$SCRIPT_DIR"
if [ "$(uname)" == 'Darwin' ]; then
    #このファイルから二つ下の階層のファイルを実行する。この際、pwdで取得したパスを使う
    for f in `find $SCRIPT_DIR/osx/*`;
    do bash $f
    done
    OS='Mac'
elif [ "$(expr substr $(uname -s) 1 5)" == 'Linux' ]; then
    OS='Linux'
elif [ "$(expr substr $(uname -s) 1 10)" == 'MINGW32_NT' ]; then                                                                                           
    OS='Cygwin'
else
    echo "Your platform ($(uname -a)) is not supported."
fi

cat << END

**************************************************
DOTFILES SETUP FINISHED! bye.
**************************************************

END

if(OS == 'Mac') then
  echo "Now you need to restart your Mac to apply the changes. SO it's comming"
  sudo reboot
fi
