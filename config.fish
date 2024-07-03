if status is-interactive
    # Commands to run in interactive sessions can go here
    alias e='emacsclient -c'
    alias ed='emacs --daemon'
    alias mg++='g++ -g -Wall -Wextra -std=c++20 -pthread'
    fish_add_path -p /home/kaiyang/.local/bin
end
