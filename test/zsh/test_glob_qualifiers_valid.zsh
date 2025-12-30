#!/bin/zsh
# Test: ZSH glob qualifiers - valid syntax that should not warn

# Glob with qualifier - all regular files
for file in *.txt(.); do
    echo "$file"
done

# Glob with multiple qualifiers - regular files, readable, not empty
for file in *.sh(.-^Lk+0); do
    echo "$file"
done

# Glob qualifier - directories only
for dir in *(/); do
    echo "$dir"
done

# Glob qualifier - symbolic links
for link in *(@); do
    echo "$link"
done

# Glob qualifier with sorting
for file in *.log(.om); do  # Regular files, ordered by modification time
    echo "$file"
done

# Multiple qualifiers
for file in *.dat(-.rwx); do  # Regular files, readable, writable, executable
    echo "$file"
done
