function ifconfig
    set interface ""
    set ether ""
    command ifconfig | while read line
        if set if (string match -r -g "^([A-Za-z0-9]+):" $line)
            set interface $line
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
