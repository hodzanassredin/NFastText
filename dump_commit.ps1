.\build.cmd
$dir = "D:\ft\dumps\"
cat .\.git\refs\heads\master | %{fsi fasttext.fsx > "$dir$_.dump" }