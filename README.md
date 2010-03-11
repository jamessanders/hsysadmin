Installation
------------

hsysadmin requires hfind from http://github.com/jamessanders/hfind

To install clone the repo and run `cabal install`

Usage
-----

renamex - renamex <regex> <replacement_text> <file_path>

recurses file_path replacing regex with with replacement_text in each file name.

Examples

    renamex "^.*(\..*)$" "test%1" ./

The above example would rename ./file.hs to ./test.hs

The above example shows that groups (parts of the regex enclosed in parethesis) can be refered to in the replacement text with a patter such as %1.
