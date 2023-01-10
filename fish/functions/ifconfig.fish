function ifconfig
    # if stdout is a terminal, just run ifconfig
    if not [ -t 1 ]
        command ifconfig $argv
        return
    end

    set interface ""
    set ether ""
    command ifconfig $argv | while read line
        if string match -rqg "^([A-Za-z0-9]+):" $line
            # remove flags=<flags> from the interface line
            set interface (string replace -r "flags=\\d+<[^>]+> *" " " $line)
            set ether ""
        else
            # first word on the line that begins with spaces
            set word (string match -r -g "^\s+([A-Za-z0-9_]+)" $line)
            switch $word
                case ether
                    set ether $line
                case inet
                    if set ip (string match -r -g "^\s+inet\s+([0-9.]+)" $line)
                        echo $interface
                        test -n $ether
                        and echo $ether
                        echo $line
                    end
            end
        end
    end
end
