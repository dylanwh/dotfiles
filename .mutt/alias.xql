declare variable $file as xs:string external;
for $contact in doc($file)//contact
	let $id     := $contact/@id
	let $name   := $contact/name
	let $groups := for $g in $contact/group return concat("-group ", $g)
	for $email in $contact/email
		let $nick := if ($email/@label)
		             then concat($id, ".", $email/@label)
		             else $id
	    return concat("alias ", string-join($groups, " "), " ", $nick, ' "', $name, '" <', $email, '>')
