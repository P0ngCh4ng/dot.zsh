zstyle ":completion:*:commands" rehash 1
autoload -U zmv
#PHPのパス
export PATH="/opt/homebrew/opt/php@7.4/bin:$PATH"
export PATH="/opt/homebrew/opt/php@7.4/sbin:$PATH"
#mysqlのパス
export PATH="/opt/homebrew/opt/mysql@5.7/bin:$PATH"
export PATH=/opt/homebrew/opt/python@3.9/libexec/bin:$PATH
#Brewのパスを通す
eval "$(/opt/homebrew/bin/brew shellenv)"

if type brew &>/dev/null; then
    FPATH=$(brew --prefix)/share/zsh-completions:$FPATH

    autoload -Uz compinit
    compinit
fi
PROMPT='%F{034}%n%f %F{036}($(arch))%f:%F{020}%~%f $(git_super_status)'
PROMPT+=""$'\n'"%# "

export ZPLUG_HOME=/opt/homebrew/opt/zplug
source $ZPLUG_HOME/init.zsh


zplug 'zsh-users/zsh-autosuggestions'
zplug 'zsh-users/zsh-completions'
zplug 'zsh-users/zsh-syntax-highlighting'
zplug "zsh-users/zsh-history-substring-search", hook-build:"__zsh_version 4.3"
zplug "woefe/git-prompt.zsh"
# 未インストール項目をインストールする
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

# コマンドをリンクして、PATH に追加し、プラグインは読み込む
zplug load --verbose
