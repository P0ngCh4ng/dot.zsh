
if [ "$(uname)" == 'Darwin' ]; then
    #このファイルから二つ下の階層のファイルを実行する。この際、pwdで取得したパスを使う
    source $(cd $(dirname $0) && pwd)/osx/install.sh
    
elif [ "$(expr substr $(uname -s) 1 5)" == 'Linux' ]; then
    OS='Linux'
elif [ "$(expr substr $(uname -s) 1 10)" == 'MINGW32_NT' ]; then                                                                                           
    OS='Cygwin'
else
    echo "Your platform ($(uname -a)) is not supported."
fi
