function s3_content_type_fix
      set -l bucket $argv[1]

      s3cmd --recursive modify --acl-public \
            --add-header='content-type':'image/png' \
            --exclude '' --include '.png' \
            $bucket
      s3cmd --recursive modify --acl-public \
            --add-header='content-type':'text/css' \
            --exclude '' --include '.css' \
            $bucket
      s3cmd --recursive modify --acl-public \
            --add-header='content-type':'text/javascript' \
            --exclude '' --include '.js' \
            $bucket
end
