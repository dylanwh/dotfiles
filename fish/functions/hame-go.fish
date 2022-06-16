function hame-go
    have go
    or return 1

    hame-echo configuring go stuff

    set --erase GOPATH
    if [ -d /opt/go ]
        set -Ux GOPATH /opt/go
    else
        mkdir -p $HOME/.local/go
        set -Ux GOPATH $HOME/.local/go
    end

    path add $GOPATH/bin

    if not have gron
        hame-echo installing gron
        hame-nq go install github.com/tomnomnom/gron@latest
    end
    if not have jsonnet
        hame-echo installing jsonnet
        hame-nq go install github.com/google/go-jsonnet/cmd/jsonnet@latest
    end
    if not have jsonnetfmt
        hame-echo installing jsonnetfmt
        hame-nq go install github.com/google/go-jsonnet/cmd/jsonnetfmt@latest
    end
end
