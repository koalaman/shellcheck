#!/bin/bash
# This script packages up compiled binaries
set -ex
shopt -s nullglob extglob

ls -l

cp ../LICENSE LICENSE.txt
sed -e $'s/$/\r/' > README.txt << END
This is a precompiled ShellCheck binary.
      https://www.shellcheck.net/

ShellCheck is a static analysis tool for shell scripts.
It's licensed under the GNU General Public License v3.0.
Information and source code is available on the website.

This binary was compiled on $(date -u).



      ====== Latest commits ======

$(git log -n 3)
END

for dir in */
do
  cp LICENSE.txt README.txt "$dir"
done

echo "Tags are $TAGS"

for tag in $TAGS
do

  for dir in windows.*/
  do
    ( cd "$dir" && zip "../shellcheck-$tag.zip" * )
  done

  for dir in {linux,darwin}.*/
  do
    base="${dir%/}"
    ( cd "$dir" && tar -cJf "../shellcheck-$tag.$base.tar.xz" --transform="s:^:shellcheck-$tag/:" * )
  done
done

for file in ./*
do
  [[ -f "$file" ]] || continue
  sha512sum "$file" > "$file.sha512sum"
done

ls -l
