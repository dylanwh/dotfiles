function fzy_select_directory --description 'cd to a directory using fzy'
  command fd . -t d | fzy | read -l dir
  if [ $dir ]
    cd $dir
  end
  commandline -f repaint
end
