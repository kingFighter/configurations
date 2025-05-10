# path: ~/.config/fish/config.fish
# based on `oh my fish`
if status is-interactive
    # Commands to run in interactive sessions can go here
    alias e='emacsclient -c'
    alias ed='emacs --daemon'
    alias mg++='g++ -g -Wall -Wextra -std=c++23'
    fish_add_path -p /home/kaiyang/.local/bin
    set -gx COLORTERM "truecolor"
end
