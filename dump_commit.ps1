$dir = "D:\ft\dumps\"
cat .\.git\ORIG_HEAD | %{fsi fasttext.fsx > "$dir$_.dump" }