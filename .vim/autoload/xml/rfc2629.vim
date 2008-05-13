let g:xmldata_rfc2629 = {
\ 'vimxmlentities': ['AElig', 'Aacute', 'Acirc', 'Agrave', 'Aring', 'Atilde', 'Auml', 'Ccedil', 'Dagger', 'ETH', 'Eacute', 'Ecirc', 'Egrave', 'Euml', 'Iacute', 'Icirc', 'Igrave', 'Iuml', 'Ntilde', 'OElig', 'Oacute', 'Ocirc', 'Ograve', 'Oslash', 'Otilde', 'Ouml', 'Prime', 'Scaron', 'THORN', 'Uacute', 'Ucirc', 'Ugrave', 'Uuml', 'Yacute', 'Yuml', 'Zcaron', 'aacute', 'acirc', 'acute', 'aelig', 'agrave', 'amp', 'apos', 'aring', 'ast', 'atilde', 'auml', 'bdquo', 'brvbar', 'bsol', 'bull', 'ccedil', 'cedil', 'cent', 'circ', 'colon', 'comma', 'commat', 'copy', 'curren', 'dagger', 'dash', 'deg', 'divide', 'dollar', 'eacute', 'ecirc', 'egrave', 'emsp', 'ensp', 'equals', 'eth', 'euml', 'euro', 'excl', 'fnof', 'frac12', 'frac14', 'frac34', 'frasl', 'ge', 'grave', 'gt', 'hArr', 'harr', 'hellip', 'hyphen', 'iacute', 'icirc', 'iexcl', 'igrave', 'iquest', 'iuml', 'lArr', 'lang', 'laquo', 'larr', 'lcub', 'ldquo', 'le', 'lowast', 'lowbar', 'lpar', 'lsaquo', 'lsqb', 'lsquo', 'lt', 'macr', 'mdash', 'micro', 'middot', 'minus', 'nbhy', 'nbsp', 'ndash', 'not', 'ntilde', 'num', 'oacute', 'ocirc', 'oelig', 'ograve', 'ordf', 'ordm', 'oslash', 'otilde', 'ouml', 'para', 'percnt', 'period', 'permil', 'plus', 'plusmn', 'pound', 'prime', 'quest', 'quot', 'rArr', 'rang', 'raquo', 'rarr', 'rcub', 'rdquo', 'reg', 'rfc.number', 'rpar', 'rsaquo', 'rsqb', 'rsquo', 'sbquo', 'scaron', 'sect', 'semi', 'shy', 'sol', 'sup1', 'sup2', 'sup3', 'szlig', 'thinsp', 'thorn', 'tilde', 'times', 'trade', 'uacute', 'ucirc', 'ugrave', 'uml', 'uuml', 'verbar', 'wj', 'yacute', 'yen', 'yuml', 'zcaron'],
\ 'vimxmlroot': ['rfc'],
\ 'abstract': [
\ ['t'],
\ { }
\ ],
\ 'address': [
\ ['postal', 'phone', 'facsimile', 'email', 'uri'],
\ { }
\ ],
\ 'annotation': [
\ ['xref', 'eref', 'iref', 'cref', 'spanx'],
\ { }
\ ],
\ 'area': [
\ [''],
\ { }
\ ],
\ 'artwork': [
\ [''],
\ { 'width': [''], 'align': ['left', 'left', 'center', 'right'], 'alt': [''], 'src': [], 'name': [''], 'type': [''], 'xml:space': ['preserve', 'default', 'preserve'], 'height': ['']}
\ ],
\ 'author': [
\ ['organization', 'address'],
\ { 'initials': [], 'fullname': [], 'role': ['editor'], 'surname': []}
\ ],
\ 'back': [
\ ['references', 'section'],
\ { }
\ ],
\ 'c': [
\ ['xref', 'eref', 'iref', 'cref', 'spanx'],
\ { }
\ ],
\ 'city': [
\ [''],
\ { }
\ ],
\ 'code': [
\ [''],
\ { }
\ ],
\ 'country': [
\ [''],
\ { }
\ ],
\ 'cref': [
\ [''],
\ { 'source': [], 'anchor': []}
\ ],
\ 'date': [
\ [],
\ { 'month': [], 'day': [], 'year': []}
\ ],
\ 'email': [
\ [''],
\ { }
\ ],
\ 'eref': [
\ [''],
\ { 'target': []}
\ ],
\ 'facsimile': [
\ [''],
\ { }
\ ],
\ 'figure': [
\ ['preamble', 'artwork', 'postamble'],
\ { 'width': [''], 'anchor': [], 'align': ['left', 'left', 'center', 'right'], 'alt': [''], 'src': [], 'title': [''], 'height': ['']}
\ ],
\ 'format': [
\ [],
\ { 'target': [], 'type': [], 'octets': []}
\ ],
\ 'front': [
\ ['title', 'author', 'date', 'area', 'workgroup', 'keyword', 'abstract', 'note'],
\ { }
\ ],
\ 'iref': [
\ [],
\ { 'subitem': [''], 'item': [], 'primary': ['false', 'true', 'false']}
\ ],
\ 'keyword': [
\ [''],
\ { }
\ ],
\ 'list': [
\ ['t'],
\ { 'style': [], 'counter': [], 'hangindent': []}
\ ],
\ 'middle': [
\ ['section'],
\ { }
\ ],
\ 'note': [
\ ['t'],
\ { 'title': []}
\ ],
\ 'organization': [
\ [''],
\ { 'abbrev': []}
\ ],
\ 'phone': [
\ [''],
\ { }
\ ],
\ 'postal': [
\ ['street', 'city', 'region', 'code', 'country'],
\ { }
\ ],
\ 'postamble': [
\ ['xref', 'eref', 'iref', 'cref', 'spanx'],
\ { }
\ ],
\ 'preamble': [
\ ['xref', 'eref', 'iref', 'cref', 'spanx'],
\ { }
\ ],
\ 'reference': [
\ ['front', 'seriesinfo', 'format', 'annotation'],
\ { 'anchor': [], 'target': []}
\ ],
\ 'references': [
\ ['reference'],
\ { 'title': ['References']}
\ ],
\ 'region': [
\ [''],
\ { }
\ ],
\ 'rfc': [
\ ['front', 'middle', 'back'],
\ { 'ipr': ['full2026', 'noDerivativeWorks2026', 'none', 'full3667', 'noModification3667', 'noDerivatives3667', 'full3978', 'noModification3978', 'noDerivatives3978'], 'number': [], 'docname': [], 'submissiontype': ['IETF', 'IETF', 'independent'], 'updates': [''], 'xml:lang': ['en'], 'seriesno': [], 'iprextract': [], 'category': ['info', 'std', 'bcp', 'info', 'exp', 'historic'], 'obsoletes': ['']}
\ ],
\ 'section': [
\ ['t', 'figure', 'texttable', 'iref', 'section'],
\ { 'anchor': [], 'toc': ['default', 'include', 'exclude', 'default'], 'title': []}
\ ],
\ 'seriesinfo': [
\ [],
\ { 'value': [], 'name': []}
\ ],
\ 'spanx': [
\ [''],
\ { 'style': ['emph']}
\ ],
\ 'street': [
\ [''],
\ { }
\ ],
\ 't': [
\ ['list', 'figure', 'xref', 'eref', 'iref', 'cref', 'spanx', 'vspace'],
\ { 'hangtext': [], 'anchor': []}
\ ],
\ 'texttable': [
\ ['preamble', 'ttcol', 'c', 'postamble'],
\ { 'anchor': [], 'title': ['']}
\ ],
\ 'title': [
\ [''],
\ { 'abbrev': []}
\ ],
\ 'ttcol': [
\ [''],
\ { 'width': [], 'align': ['left', 'left', 'center', 'right']}
\ ],
\ 'uri': [
\ [''],
\ { }
\ ],
\ 'vspace': [
\ [],
\ { 'blanklines': ['0']}
\ ],
\ 'workgroup': [
\ [''],
\ { }
\ ],
\ 'xref': [
\ [''],
\ { 'pageno': ['false', 'true', 'false'], 'target': [], 'format': ['default', 'counter', 'title', 'none', 'default']}
\ ],
\ 'vimxmltaginfo': {
\ 'date': ['/>', ''],
\ 'format': ['/>', ''],
\ 'iref': ['/>', ''],
\ 'seriesinfo': ['/>', ''],
\ 'vspace': ['/>', ''],
\ }
\ }
" vim:ft=vim:ff=unix