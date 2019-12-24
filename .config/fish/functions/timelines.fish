function timelines
  set -l files
  for input in $argv
    set -l file (mktemp)
    sort $input | uniq -c |awk '{print $2, $1}' | sort > $file
    set -a files $file
  end

  set -l cleanup $files
  while [ (count $files) -ge 2 ]
    set -l temp (mktemp)
    set -a cleanup $temp
    join -e NULL -j 1 $files[1] $files[2] > $temp
    set -e files[1 2]
    set -p files $temp
  end
  echo "value" $argv
  cat $files[1]
  command rm -f $cleanup
end
