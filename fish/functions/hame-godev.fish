function hame-godev
    have gopls
    or hame-nq env GO111MODULE=on go get golang.org/x/tools/gopls@latest

    have golangci-lint
    or hame-nq env GO111MODULE=on CGO_ENABLED=0 go get -trimpath -ldflags '-s -w' github.com/golangci/golangci-lint/cmd/golangci-lint 

    have godoc
    or hame-nq go get golang.org/x/tools/cmd/godoc

    have goimports
    or hame-nq go get golang.org/x/tools/cmd/goimports

    have gorename
    or hame-nq go get golang.org/x/tools/cmd/gorename

    have guru
    or hame-nq go get golang.org/x/tools/cmd/guru

    have gotests
    or hame-nq go get github.com/cweill/gotests/...

    have fillstruct
    or hame-nq go get github.com/davidrjenni/reftools/cmd/fillstruct

    have gomodifytags
    or hame-nq go get github.com/fatih/gomodifytags

    have godoctor
    or hame-nq go get github.com/godoctor/godoctor

    have gopkgs
    or hame-nq go get github.com/haya14busa/gopkgs/cmd/gopkgs

    have impl
    or hame-nq go get github.com/josharian/impl

    have gocode
    or hame-nq go get github.com/mdempsky/gocode

    have godef
    or hame-nq go get github.com/rogpeppe/godef

    have gogetdoc
    or hame-nq go get github.com/zmb3/gogetdoc
end
