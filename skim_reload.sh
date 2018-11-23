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

exec /usr/bin/osascript << EOF
  set theFile to POSIX file "${pdffile}" as alias
  tell application "Skim"
  if ${activate} then activate
  set theDocs to get documents whose path is (get POSIX path of theFile)
  if (count of theDocs) > 0 then revert theDocs
  open theFile
  end tell		   
EOF
