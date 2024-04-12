zstyle ":completion:*:commands" rehash 1
autoload -U zmv
PATH="$HOME/homebrew/bin:$PATH"
ZPLUG_HOME="$HOME/.zplug"
VOLT_HOME="$HOME/.volta"


# brewがない場合
if type brew &>/dev/null; then
    FPATH=$(brew --prefix)/share/zsh-completions:$FPATH

    autoload -Uz compinit
    compinit -i
fi
if [ -e ~/.zshrc.local ]; then
    source ~/.zshrc.local
fi
source $ZPLUG_HOME/init.zsh
source $(pwd)/dotfiles/opt.zsh  #todo

PROMPT='%F{034}%n%f %F{036}($(arch))%f:%F{020}%~%f $(git_super_status)'
PROMPT+=""$'\n'"%# "

alias ls='lsd'
alias l='lsd -l'
alias ll='lsd -l'
alias la='lsd -a'
alias lla='lsd -la'
alias lt='lsd --tree'


# プラグインの指定_

zplug 'zsh-users/zsh-autosuggestions'
zplug 'zsh-users/zsh-completions'
zplug 'zsh-users/zsh-syntax-highlighting'
zplug "zsh-users/zsh-history-substring-search"
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
export VOLTA_HOME="$HOME/.volta"
export PATH="$VOLTA_HOME/bin:$PATH"

[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env" # ghcup-envexport PATH="/opt/homebrew/opt/llvm/bin:$PATH"
