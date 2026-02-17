#!/bin/bash
############################
# .make.sh
# This script creates symlinks from the home directory to any desired dotfiles in ~/dotfiles
############################

########## Variables
dir="$HOME/dotfiles"        # dotfiles directory
olddir="$HOME/dotfiles_old" # old dotfiles backup directory

# 처리할 파일과 디렉토리를 분리
files="vimrc zshrc tmux.conf ideavimrc zprofile wezterm.lua doom.d emacs.d/init.el"
dirs="config/nvim config/ghostty config/yazi config/karabiner"
##########

# create dotfiles_old in homedir
echo "Creating $olddir for backup of any existing dotfiles in ~"
mkdir -p "$olddir"
echo "...done"

# change to the dotfiles directory
echo "Changing to the $dir directory"
cd "$dir" || exit
echo "...done"

# ==========================================
# 1. 파일(Files) 처리
# ==========================================
echo "Processing files..."
for file in $files; do
  echo "Processing file $file..."

  # A. dotfiles 저장소에 해당 파일이 없는 경우 (새로 추가할 때)
  if [ ! -e "$dir/$file" ]; then
    echo "  [알림] dotfiles에 $file 이(가) 없습니다."
    # 홈 디렉토리에 진짜 파일이 존재한다면, 백업하지 않고 dotfiles로 '이동'시켜 관리를 시작합니다.
    if [ -e "$HOME/.$file" ] && [ ! -L "$HOME/.$file" ]; then
      echo "  -> 기존 로컬 파일을 dotfiles 저장소로 이동하여 깃(Git) 관리를 시작합니다."
      mkdir -p "$(dirname "$dir/$file")"
      mv "$HOME/.$file" "$dir/$file"
    else
      echo "  -> 빈 파일을 생성합니다."
      mkdir -p "$(dirname "$dir/$file")"
      touch "$dir/$file"
    fi
  fi

  # B. 홈 디렉토리에 기존 파일이 있고, 그것이 '심볼릭 링크'가 아닐 경우에만 백업
  # (스크립트를 여러 번 실행해도 링크가 꼬이지 않도록 방지)
  if [ -e "$HOME/.$file" ] && [ ! -L "$HOME/.$file" ]; then
    echo "  Moving existing file $HOME/.$file to $olddir"
    mkdir -p "$olddir/$(dirname "$file")"
    mv "$HOME/.$file" "$olddir/$file"
  fi

  # C. 심볼릭 링크 생성
  echo "  Creating symlink for file $file"
  mkdir -p "$HOME/.$(dirname "$file")"
  ln -sfn "$dir/$file" "$HOME/.$file"
done

# ==========================================
# 2. 디렉토리(Directories) 처리
# ==========================================
echo "Processing directories..."
for dir_item in $dirs; do
  echo "Processing directory $dir_item..."

  # A. dotfiles 저장소에 해당 폴더가 없는 경우 (새로 추가할 때)
  if [ ! -d "$dir/$dir_item" ]; then
    echo "  [알림] dotfiles에 $dir_item 폴더가 없습니다."
    # 홈 디렉토리에 진짜 폴더가 존재한다면, dotfiles로 통째로 '이동'시킵니다.
    if [ -e "$HOME/.$dir_item" ] && [ ! -L "$HOME/.$dir_item" ]; then
      echo "  -> 기존 로컬 폴더를 dotfiles 저장소로 이동하여 깃(Git) 관리를 시작합니다."
      mkdir -p "$(dirname "$dir/$dir_item")"
      mv "$HOME/.$dir_item" "$dir/$dir_item"
    else
      echo "  -> 빈 폴더를 생성합니다."
      mkdir -p "$dir/$dir_item"
    fi
  fi

  # B. 홈 디렉토리에 부모 폴더가 존재하는지 확인 및 생성 (예: ~/.config)
  mkdir -p "$HOME/.$(dirname "$dir_item")"

  # C. 홈 디렉토리에 기존 폴더가 있고, 그것이 '심볼릭 링크'가 아닐 경우에만 백업
  if [ -e "$HOME/.$dir_item" ] && [ ! -L "$HOME/.$dir_item" ]; then
    echo "  Moving existing directory $HOME/.$dir_item to $olddir"
    mkdir -p "$olddir/$(dirname "$dir_item")"
    mv "$HOME/.$dir_item" "$olddir/$dir_item"
  fi

  # D. 심볼릭 링크 생성 (원본 절대경로 -> 대상 절대경로)
  echo "  Creating symlink for directory $dir_item"
  ln -sfn "$dir/$dir_item" "$HOME/.$dir_item"
done

echo "🎉 All files and directories have been successfully processed!"
