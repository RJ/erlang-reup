#!/bin/bash -e
DIR="$1"
if [ ! -d "$DIR" ]
then
    >&2 echo "reup error: can't watch missing dir $DIR"
    exit 1
fi
# we touch this file, then ask find to find files modified more recently
# two files are used to avoid a touch->(save .erl)->find race condition
MARKER=$(mktemp -t tmp.XXXXXXXXXX)
a=0
b=1
touch "${MARKER}.$a"
touch "${MARKER}.$b"

function cleanup() {
    rm -f "${MARKER}.0"
    rm -f "${MARKER}.1"
}
trap cleanup EXIT

# loop is pumped by erlang process writing to stdin
# so we can control polling speed from erlang without overlapping under load
# 60 second timeout on read, in case erlang exits uncleanly.
while read -t 60 line
do
    if [ "$line" = "exit" ] || [ -z "$line" ]
    then
        exit 0
    fi

    if [ "$line" != "pump" ]
    then
        >&2 echo "reup invalid input line: $line"
        exit 2
    fi

    touch "${MARKER}.$a"
    find "$DIR" -newer "${MARKER}.$b" -type f | while read f
    do
        if [[ "$f" == *hrl ]]; then
            # if a .hrl file is changed, emit all .erl files that mention it
            grep -nr "$(basename "$f")\"" "$DIR" | awk -F ':' '{print $1}'
        elif [[ "$f" == *erl ]]; then
            # else just compile the erl file
            echo "$f"
        fi
    done
    # swap i & ii
    tmp="$a"
    a="$b"
    b="$tmp"
    # tell erlang we're done, so it can enqueue the next poll
    echo "ok"
done
