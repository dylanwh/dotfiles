# Defined in - @ line 1
function home --wraps='cd ~/Git/dylanwh/home' --description 'alias home cd ~/Git/dylanwh/home'
    cd ~/Git/dylanwh/home
    if [ -n "$argv" ]
        $argv
    end
end
