#########################################################################
#### SemiAutoTeX 
#### Time-stamp: <2012-02-25 05:36:14 seto>
#########################################################################
## You can find some explanations at
## http://d.hatena.ne.jp/setoryohei/20120219
#########################################################################
#!/bin/sh
if [ $# == 0 -o "$1" == "-h" -o "$1" == "-help"  ]; then
    echo "Usage: semiautotex [-b] [-i] [-pd] TEXFILE 
Options:
-b          run BibTeX with LaTeX
-i          run MakeIndex with LaTeX
-pd         generate pdf by dvipdf
-pl         use platex"
    exit 0
fi

mode="tex"
pdfdvi=0
use_platex=0

while [ "${1:0:1}" == "-" ]; do
    if [ "$1" == "-b" ]; then
	mode="bib"
    elif [ "$1" == "-i" ]; then
	mode="idx"
    elif [ "$1" == "-pd" ]; then
	pdfdvi=1
    elif [ "$1" == "-pl" ]; then
	use_platex=1
    fi
    shift
done

if [ $use_platex = 1 ]; then
## For platex
    LATEX="platex"
    LATEXDRAFT="platex"
    BIBTEX="pbibtex"
    MAKEINDEX="mendex -U"
    DVIPDF="dvipdfmx"
else
## For PDFLaTeX
    LATEX="pdflatex -synctex=1"
    LATEXDRAFT="pdflatex -draftmode"
    BIBTEX="bibtex"
    MAKEINDEX="makeindex"
    DVIPDF=""
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
	    while checksum_before="$checksum" \
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
	    $LATEXDRAFT $texfile \
		&& $LATEX $texfile
	    message="typeset + BibTeX + typeset + typeset"
	fi
	;;
    "idx" )
	checksum=`md5 -q $texfile.aux`
	$LATEXDRAFT $texfile \
	    && typset_pass=1
	if [ $typset_pass = 1 ]; then
	    message="typeset"
	    while checksum_before="$checksum" \
		checksum=`md5 -q $texfile.aux` && \
		[ "$checksum" != "$checksum_before" ]; do
		$LATEXDRAFT $texfile 
		message=`echo "$message + typeset"`
	    done
	    $MAKEINDEX $texfile 
	    $LATEX $texfile 
	    message=`echo "$message + MakeIndex + typeset"`
	fi
esac

if [ $typset_pass = 1 ]; then
    if [ $pdfdvi = 1 ]; then
	$DVIPDF $texfile 
	message=`echo "$message + DVIPDF"`
    fi
    echo "SemiAutoTeX: $message"
else 
    echo "SemiAutoTeX: failed"
    exit 1
fi
