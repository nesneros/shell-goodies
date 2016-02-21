#! /usr/bin/env zsh -e

mkdir -p $HOME/.goodies

# Set git global template

local dir="${0:h:A}"

cat <<EOF

#######################################################
#######################################################
Perform the following manual steps:
- Ensure '$dir/zshenv.zsh' is sourced by '$HOME/.zshenv'
- Ensure '$dir/zshrc.zsh' is sourced by '$HOME/.zshrc'
The latter can be done with:
  source \${SHELL_GOODIES_ROOT}/zshrc.zsh
#######################################################
#######################################################

EOF
