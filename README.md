Installation
------------

hsysadmin requires hfind from http://github.com/jamessanders/hfind

To install clone the repo and run `cabal install`

renamex
-------

renamex - renamex <regex> <replacement_text> <file_path>

recurses file_path replacing regex with with replacement_text in each file name.

Examples

    renamex "^.*(\..*)$" "test%1" ./

The above example would rename ./file.hs to ./test.hs

The above example shows that groups (parts of the regex enclosed in parethesis) can be refered to in the replacement text with a patter such as %1.


chronos
-------

Usage:

    chronos interval 10 launch.sh               # Will run the command every 10 seconds
    chronos daily 1:00,17:00 launch.sh          # Will run the command at 1:00am and 5:00pm every day
    chronos weekly Fri,Sat 1:00,17:00 launch.sh # Will run the command at 1:00am and 5:00pm every Friday and Saturday

Thats it.