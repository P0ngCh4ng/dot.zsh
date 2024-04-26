zstyle ":completion:*:commands" rehash 1
autoload -U zmv



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

alias g='git'
alias ga='git add'
alias gd='git diff'
alias gs='git status'
alias gp='git push'
alias gb='git branch'
alias gst='git status'
alias gco='git checkout'
alias gf='git fetch'
alias gc='git commit'

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
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


[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env" # ghcup-envexport PATH="/opt/homebrew/opt/llvm/bin:$PATH"
