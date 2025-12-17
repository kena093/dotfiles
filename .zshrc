##### Locale #####
export LANG=ja_JP.UTF-8
export LC_ALL=ja_JP.UTF-8

##### PATH（配列で重複除去＆先頭優先） #####
typeset -U path PATH
path=(
  /opt/homebrew/bin
  /usr/local/bin
  /opt/homebrew/opt/openjdk/bin
  $path
)

##### Aliases / Tools #####
# Doom Emacsなどの外部スクリプトがEmacsのパスを認識できるように環境変数を設定
export EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
export acmeshell=/bin/bash   # acmeのシェル差し替え

##### Zsh options #####
setopt histignorealldups
setopt auto_cd
setopt auto_pushd
setopt nolistbeep

##### Colors / Completion #####
autoload -Uz colors; colors
autoload -Uz compinit
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "$HOME/.zsh/cache"
zstyle ':completion:*:default' menu select=1
zstyle ':completion:*:commands' rehash 1
[[ -d $HOME/.zsh/cache ]] || mkdir -p "$HOME/.zsh/cache"
compinit -i

##### Editor / Reporting #####
export EDITOR=emacs
export REPORTTIME=1

##### Starship only（自作PROMPTやprecmdは使わない） #####
export USE_STARSHIP=1
eval "$(starship init zsh)"

##### Local overrides（存在時のみ読み込み） #####
[[ -r "${HOME}/.zsh/.zshrc.local" ]] && source "${HOME}/.zsh/.zshrc.local"

# Added by Antigravity
export PATH="/Users/kena/.antigravity/antigravity/bin:$PATH"

# Homebrewのパスを優先的に設定
export PATH="/opt/homebrew/bin:$PATH"
