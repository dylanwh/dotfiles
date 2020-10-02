# Defined in - @ line 1
function hame-nq --wraps="env NQDIR=$HOME/.cache/hame nq -c -q" --description 'alias hame-nq env NQDIR=~/.cache/hame nq -c -q'
  echo "$argv"
  env NQDIR=$HOME/.cache/hame nq -c -q $argv;
end
