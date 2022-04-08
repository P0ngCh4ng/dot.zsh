#PHPのパス
export PATH="/opt/homebrew/opt/php@7.4/bin:$PATH"
export PATH="/opt/homebrew/opt/php@7.4/sbin:$PATH"
#mysqlのパス
export PATH="/opt/homebrew/opt/mysql@5.7/bin:$PATH"
#pythonのパスを通す(zshrcでいいのでは？)
export PATH=/opt/homebrew/opt/python@3.9/libexec/bin:$PATH
#Brewのパスを通す
eval "$(/opt/homebrew/bin/brew shellenv)"
