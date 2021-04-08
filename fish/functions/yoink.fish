# Defined via `source`
function yoink --wraps='open -a Yoink' --description 'alias yoink open -a Yoink'
  open -a Yoink $argv; 
end
