#!/bin/bash


cat << START

**************************************************
THIS IS DOTFILES SETUP START SCRIPT
**************************************************

START




SCRIPT_DIR="$(cd $(dirname $0); pwd)"
echo "SCRIPT_DIR=$SCRIPT_DIR"
if [ "$(uname)" == 'Darwin' ]; then
    #このファイルから二つ下の階層のファイルを実行する。この際、pwdで取得したパスを使う
    for f in `find $SCRIPT_DIR/osx/*`;
    do bash $f
    done
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
