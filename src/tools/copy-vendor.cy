#!cyber
use os
use io

-- Usage:
-- ./copy-vendor.cy -dst=./lib/tcc -src=/Users/fubar/repos/tinycc

args := os.parseArgs({'src', 'dst'})
dst := args.at('dst')
src := args.at('src')

contents := os.read_file_str("%{dst}/vendor_files.txt")!
reader := io.Reader(contents)
scanner := io.Scanner(&reader)
while scanner.next_line()! |path|:
    path_s := str(path)
    if path_s.starts_with('--'):
        print("skip %{path_s}")
    else:
        print("copy %{src}%{path_s} %{dst}/vendor%{path_s}")
        os.copy_file("%{src}%{path_s}", "%{dst}/vendor%{path_s}")!