#########################################################################
#### SemiAutoTeX ver 0.01
#### Time-stamp: <2012-02-26 12:07:52 seto>
#########################################################################
# You can find some explanations at
# http://d.hatena.ne.jp/setoryohei/20120219
#
# The rc file '.semiautotexrc' should be put at your home directory.
# Example of '~/.semiautotexrc'
# latex="pdflatex -synctex=1"
# latexdraft="pdflatex -draftmode"
# bibtex="bibtex"
# makeindex="makeindex"
# dvipdf=""
#########################################################################
#!/bin/sh
if [ $# == 0 -o "$1" == "-h" -o "$1" == "-help"  ]; then
    echo "SemiAutoTeX 0.01:  Semi-automatic LaTeX document generation routine
Usage: semiautotex [-b] [-i] TEXFILE 
Options:
-b          run BibTeX with LaTeX
-i          run MakeIndex with LaTeX"
    exit 0
fi

mode="tex"
while [ "${1:0:1}" == "-" ]; do
    if [ "$1" == "-b" ]; then
	mode="bib"
    elif [ "$1" == "-i" ]; then
	mode="idx"
    fi
    shift
done

rcfile="${HOME}/.semiautotexrc"
if [ ! -f $rcfile ]; then
    touch $rcfile
    echo 'latex="platex"
latexdraft="platex"
bibtex="pbibtex"
makeindex="mendex -U"
dvipdf="dvipdfmx"' > $rcfile
    echo "SemiAutoTeX: rc file ($rcfile) is generated."
fi

DVIPDF=""
LATEXDRAFT=""

while read LINE
do
    if [ "${LINE:0:1}" != "#" ]; then
	case "`echo ${LINE} | cut -d"=" -f1`" in
	    "latex" ) 
		LATEX="`echo ${LINE} | cut -d'\"' -f2`";;
	    "latexdraft" )
		LATEXDRAFT="`echo ${LINE} | cut -d'\"' -f2`";;
	    "bibtex" )
		BIBTEX="`echo ${LINE} | cut -d'\"' -f2`";;
	    "makeindex" )
		MAKEINDEX="`echo ${LINE} | cut -d'\"' -f2`";;
	    "dvipdf" )
		DVIPDF="`echo ${LINE} | cut -d'\"' -f2`";;
	esac
    fi
done <$rcfile

if [ "$LATEXDRAFT" == "" ]; then
    LATEXDRAFT="$LATEX"
fi

texfile=`basename $1 .tex`
typset_pass=0 

case "$mode" in
    "tex" ) 
	if [ -f $texfile.aux ]; then
	    checksum=`md5 -q $texfile.aux`
	    $LATEX $texfile && typset_pass=1 
	else
	    $LATEXDRAFT $texfile && typset_pass=1
	fi
	if [ $typset_pass = 1 ]; then
	    message="typeset"
	    while checksum_before="$checksum" && \
		checksum=`md5 -q $texfile.aux` && \
		[ "$checksum" != "$checksum_before" ]; do
		$LATEX $texfile 
		message=`echo "$message + typeset"`
	    done
	fi 
	;;
    "bib" )
	$LATEXDRAFT $texfile && typset_pass=1
	if [ $typset_pass = 1 ]; then
	    $BIBTEX $texfile 
	    $LATEXDRAFT $texfile && $LATEX $texfile
	    message="typeset + BibTeX + typeset + typeset"
	fi
	;;
    "idx" )
	checksum=`md5 -q $texfile.aux`
	$LATEXDRAFT $texfile && typset_pass=1
	if [ $typset_pass = 1 ]; then
	    message="typeset"
	    while checksum_before="$checksum" && \
		checksum=`md5 -q $texfile.aux` && \
		[ "$checksum" != "$checksum_before" ]; do
		$LATEXDRAFT $texfile 
		message=`echo "$message + typeset"`
	    done
	    $MAKEINDEX $texfile 
	    $LATEX $texfile 
	    message=`echo "$message + MakeIndex + typeset"`
	fi
	;;
esac

if [ $typset_pass = 1 ]; then
    if [ "$DVIPDF" != "" ]; then
	$DVIPDF $texfile 
	message=`echo "$message + DVIPDF"`
    fi
    echo "SemiAutoTeX: $message"
else 
    echo "SemiAutoTeX: failed"
    exit 1
fi
