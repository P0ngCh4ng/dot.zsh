#!/bin/bash
#macOSの基本的な設定を記述予定

echo "Installing Xcode Command Line Tools..."
xcode-select --install


# Show the ~/Library folder
defaults write NSGlobalDomain AppleShowAllExtensions -bool true



# Do not create .DS_Store files on USB or network storage
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
defaults write com.apple.desktopservices DSDontWriteUSBStores -bool true

# Show hidden files in Finder
defaults write com.apple.finder AppleShowAllFiles -bool true

# Hide the battery percentage from the menu bar
defaults write com.apple.menuextra.battery ShowPercent -string "NO"


# Show Scrollbars Always
defaults write -g AppleShowScrollBars -string "Always"

# Disable dialog box when crashed
defaults write com.apple.CrashReporter DialogType -string "none"    

# Disable the warning when opening an app for the first time
defaults write com.apple.LaunchServices LSQuarantine -bool false 
 
# Disable the warning before emptying the Trash
defaults write com.apple.finder WarnOnEmptyTrash -bool false

# Show all filename extensions
defaults write -g AppleShowAllExtensions -bool true

# DO NOT SHOW DOCK
defaults write com.apple.dock autohide-delay -float 100
defaults write com.apple.dock autohide -bool true
