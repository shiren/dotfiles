#!/bin/bash
############################
# .make.sh
# This script creates symlinks from the home directory to any desired dotfiles in ~/dotfiles
############################

########## Variables

dir=~/dotfiles        # dotfiles directory
olddir=~/dotfiles_old # old dotfiles backup directory

# 처리할 파일과 디렉토리를 분리
files="vimrc zshrc tmux.conf ideavimrc zprofile wezterm.lua doom.d emacs.d/init.el"
dirs="config/nvim config/ghostty"

##########

# create dotfiles_old in homedir
echo "Creating $olddir for backup of any existing dotfiles in ~"
mkdir -p $olddir
echo "...done"

# change to the dotfiles directory
echo "Changing to the $dir directory"
cd $dir
echo "...done"

# Process files
echo "Processing files..."
for file in $files; do
  echo "Processing file $file..."

  # Backup existing file
  if [ -e ~/.$file ]; then
    echo "Moving existing file ~/.$file to $olddir"
    mv ~/.$file $olddir/
  fi

  # Create symlink
  echo "Creating symlink for file $file"
  ln -sf $dir/$file ~/.$file
done

# Process directories
echo "Processing directories..."
for dir_item in $dirs; do
  echo "Processing directory $dir_item..."

  # Ensure parent directories exist
  mkdir -p ~/.$(dirname $dir_item)

  # Backup existing directory
  if [ -e ~/.$dir_item ]; then
    echo "Moving existing directory ~/.$dir_item to $olddir"
    mv ~/.$dir_item $olddir/
  fi

  # Create symlink
  echo "Creating symlink for directory $dir_item"
  echo "ln -sfn $dir/$dir_item ~/.$(dirname $dir_item)/$(basename $dir_item)"
  ln -sfn $dir/$dir_item ~/.$(dirname $dir_item)/$(basename $dir_item)
done

echo "All files and directories have been processed."
