#!/bin/bash

echo "installing homebrew..."
which brew >/dev/null 2>&1 || /usr/bin/ruby -e "$(curl -L https://github.com/Homebrew/brew/tarball/master | tar xz --strip 1 -C homebrew)"

echo "ok. run brew upgrade..."

brew bundle --global

echo "run brew doctor..."
which brew >/dev/null 2>&1 && brew doctor

echo "run brew update..."
which brew >/dev/null 2>&1 && brew update
brew cleanup

cat << END

**************************************************
HOMEBREW INSTALLED! bye.
**************************************************

END
