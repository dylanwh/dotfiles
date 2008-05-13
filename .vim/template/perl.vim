function! s:BuildPL()
append
use Module::Build;

my $build = new Module::Build (
    module_name => 'My::Module',
    license     => 'perl',
    requires    => { },
);

$build->create_build_script();
.
endfunction

function! s:UseStrict()
1append
use strict;
use warnings;
.
endfunction

let s:ext = expand("%:e")
if s:ext == 'pm'
	let s:name = substitute(expand("%:p:r"), '.*/lib/', "", "")
	if match(s:name, "^/") == 0
		let s:name = substitute(s:name, '[^A-Z]*/\([A-Z]\)', "\\1", "")
	end
	call append(0, "package " . substitute(s:name, "/", "::", "g") . ";")
	if $PERL_USE_MOOSE || exists('perl_use_moose')
		call append(1, "use Moose;")
	else
		call s:UseStrict()
	endif
	call append(line("$"), "our $VERSION = '0.01';")
	call append(line("$"), "")
	call append(line("$"), '1;')
else
	call append(0, '#!/usr/bin/perl')
	call s:UseStrict()

	if expand("%:t") == 'Build.PL'
		call s:BuildPL()
	endif
endif

let l = line("$")
exec "norm " . (l - 1) . "G"
