# Defined in - @ line 1
function ql --wraps='qlmanage -p' --description 'alias ql qlmanage -p'
  qlmanage -p $argv 2>/dev/null
end
