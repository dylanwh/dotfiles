function hame-godev
    have go
    or return 1

    hame-echo installing go development tools

    if not have gore
        hame-echo installing gore
        hame-nq go install github.com/mdempsky/gocode@latest
        hame-nq go install github.com/x-motemen/gore/cmd/gore@latest
    end

    have gopls
    or hame-nq env GO111MODULE=on go install golang.org/x/tools/gopls@latest

    have golangci-lint
    or hame-nq env GO111MODULE=on CGO_ENABLED=0 go install -trimpath -ldflags '-s -w' github.com/golangci/golangci-lint/cmd/golangci-lint@latest

    have godoc
    or hame-nq go install golang.org/x/tools/cmd/godoc@latest

    have goimports
    or hame-nq go install golang.org/x/tools/cmd/goimports@latest

    have gorename
    or hame-nq go install golang.org/x/tools/cmd/gorename@latest

    have guru
    or hame-nq go install golang.org/x/tools/cmd/guru@latest

    have gotests
    or hame-nq go install github.com/cweill/gotests/...@latest

    have fillstruct
    or hame-nq go install github.com/davidrjenni/reftools/cmd/fillstruct@latest

    have gomodifytags
    or hame-nq go install github.com/fatih/gomodifytags@latest

    have godoctor
    or hame-nq go install github.com/godoctor/godoctor@latest

    have gopkgs
    or hame-nq go install github.com/haya14busa/gopkgs/cmd/gopkgs@latest

    have impl
    or hame-nq go install github.com/josharian/impl@latest

    have gocode
    or hame-nq go install github.com/mdempsky/gocode@latest

    have godef
    or hame-nq go install github.com/rogpeppe/godef@latest

    have gogetdoc
    or hame-nq go install github.com/zmb3/gogetdoc@latest

    have iferr
    or hame-nq go install github.com/koron/iferr@latest


end
