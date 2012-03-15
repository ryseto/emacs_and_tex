#!/bin/bash
# Usage: skim_reload.sh [-g] PDFFILE
if [ $# == 0 -o "$1" == "-h" -o "$1" == "-help" ]; then
    echo "Usage: skim_reload.sh [-g] PDFFILE
Options:
-g, -background  Do not bring Skim to the foreground"
    exit 0
fi

activate=true
while [ "${1:0:1}" == "-" ]; do
    if [ "$1" == "-g" -o "$1" == "-background" ]; then
	activate=false
    fi
    shift
done

pdffile="$1"
[ "${pdffile:0:1}" == "/" ] || file="${PWD}/${pdffile}"

/usr/bin/osascript \
    -e "set theFile to POSIX file \"${pdffile}\" as alias" \
    -e "set thePath to POSIX path of theFile" \
    -e "tell application \"Skim\"" \
    -e "  if $activate then activate" \
    -e "  set theDocs to get documents whose path is thePath" \
    -e "  try" \
    -e "    if (count of theDocs) > 0 then revert theDocs" \
    -e "  end try" \
    -e "end tell"
