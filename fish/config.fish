# Homebrew
set -x PATH ""(brew --prefix)"/bin" $PATH

# expose PATH to graphical apps
launchctl setenv PATH $PATH

# Node
source ~/.config/fish/nvm-wrapper/nvm.fish
nvm use stable


