# Time-stamp: <2024-09-27 18:07:58 daniel>
#
# Author: Daniel Sjöblom
#
# This file/program is released into the public domain AS IS, with no
# guarantees of functionality or useability.  You use it at your own
# risk.

# .zshrc, based on various sources, including .bashrc from gentoo and
# ideas from zsh wiki (at http://zshwiki.org/)

if [[ $- != *i* ]] ; then
    # Shell is non-interactive.  Be done now
    return
fi

# Umask 077 unless root

if [[ ${EUID} != 0 ]]; then
    umask 077;
fi

# Set a stylish prompt for user, and an ugly one for root
if [[ ${EUID} != 0 ]]; then
    export PS1="[$(print '%{\e[1;32m%}%n@%m%{\e[0m%}:%{\e[1;34m%}%d%{\e[0m%}%{\e[1;33m%} %#%{\e[0m%}')]";
else
    export PS1='[%n@%m:%d%#]'
fi

# lesspipe (ubuntu)
if which lesspipe &> /dev/null; then
    eval "$(lesspipe)";
fi

# Some common aliases
alias ls='ls --color=auto'
alias cd..='cd ..'
alias less='less -P "%f lines %lt-%lb/%L %pm"'
alias ungrep='grep -E -v'

# For Ruby/Rails development
alias rbgrep="grep -n -s -R --include='*.rb' --exclude-dir=node_modules"
alias erbgrep="grep -n -s -R --include='*.erb' --include='*.slim' --include='*.prawn' --include='*.jbuilder'"
alias cssgrep="grep -n -s -R --include='*.scss' --include='*.sass'"
alias jsgrep="ack --nocolor --noheading -s --type-set=coffee:ext:coffee --type-set=vue:ext:vue --type=js --type=vue --type=coffee --ignore-dir='public' --ignore-dir='docs' --ignore-dir=tmp --ignore-dir=vendor --ignore-dir=coverage"
alias agrep="ack --nocolor --noheading --ignore-dir=tmp --ignore-dir=log --ignore-dir=coverage --ignore-file=ext:svg --ignore-dir=.yarn"

function i18ngrep() {
    grep -n -s -R --include='*.yml' $1 config/locales
}

function awsgrep() {
    grep -n -s -R $1 env aws
}

# bundle open will open in current emacs session
export BUNDLER_EDITOR=emacsclient

# Markdown viewer
function viewmd() {
    pandoc $1 > /tmp/$1.html
    xdg-open /tmp/$1.html
}

# Emacs keybindings
bindkey -e

# Set up history
HISTSIZE=2000
SAVEHIST=2000
HISTFILE=~/.zsh_history

# Ignore duplicates in history
setopt HIST_IGNORE_ALL_DUPS
# Append to history instead of overwriting
setopt APPEND_HISTORY

# Set up dirstack

# Push directories on cd
setopt AUTO_PUSHD
# Ignore duplicates
setopt PUSHD_IGNORE_DUPS
# Don't show list after pushd
setopt PUSHD_SILENT
# Keep max 16 entries
DIRSTACKSIZE=16

zstyle ':completion:*:(rm|kill|diff):*' ignore-line yes

# The following lines were added by compinstall

zstyle ':completion:*' completer _expand _complete
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' use-compctl false
zstyle :compinstall filename ~/.zshrc

autoload -Uz compinit
compinit

# End of lines added by compinstall

# Set RPROMPT to exit status of last command

function precmd() {
    if [ $? -eq 0 ]; then
        RPROMPT="$(print "%{\e[1;32m%}$?%{\e[0m%}")";
    else
        RPROMPT="$(print "%{\e[1;31m%}$?%{\e[0m%}")";
    fi
}

# load additional configuration specific to this host (e.g. secret keys)
if [[ -e ~/.machinerc ]]; then
    source ~/.machinerc
fi
