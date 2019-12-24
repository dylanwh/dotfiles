function rule --argument email
	echo "if address :contains \"From\" \"$email\" {"
 echo "fileinto \"INBOX.bulk\";"
echo "}"
end
