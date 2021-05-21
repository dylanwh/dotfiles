# Defined via `source`
function http --wraps=curlie --description 'alias http=curlie'
  curlie $argv; 
end
