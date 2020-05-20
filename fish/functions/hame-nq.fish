# Defined in - @ line 1
function hame-nq --wraps='env NQDIR=/Users/dylan/.cache/hame nq -c -q' --description 'alias hame-nq env NQDIR=/Users/dylan/.cache/hame nq -c -q'
  echo "$argv"
  env NQDIR=/Users/dylan/.cache/hame nq -c -q $argv;
end
