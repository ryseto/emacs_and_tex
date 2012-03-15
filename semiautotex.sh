################################################################################
#### SemiAutoTeX ver 0.02
################################################################################
# You can find some explanations at
# http://d.hatena.ne.jp/setoryohei/20120219
#
# You can put a rc file '.semiautotexrc' in your home directory.
# If there is a rc file in current directly, it has the priority.
# Example of '~/.semiautotexrc'
# latex="pdflatex -synctex=1"
# latexdraft="pdflatex -draftmode"
# bibtex="bibtex"
# makeindex="makeindex"
# dvipdf=""
# pdfviewer="skim_reload.sh -g"
################################################################################
#!/bin/sh
if [ $# == 0 -o "$1" == "-h" -o "$1" == "-help"  ]; then
    echo "SemiAutoTeX 0.02:  Semi-automatic LaTeX document generation routine
Usage: semiautotex [-b] [-i] [-pv] TEXFILE 
Options:
-b          run BibTeX with LaTeX
-i          run MakeIndex with LaTeX
-pv         open or reload PDF file"
    exit 0
fi

mode="tex"
pdfviewmode=0;
while [ "${1:0:1}" == "-" ]; do
    if [ "$1" == "-b" ]; then
	mode="bib"
    elif [ "$1" == "-i" ]; then
	mode="idx"
    elif [ "$1" == "-pv" ]; then
	pdfviewmode=1;
    fi
    shift
done

rcfile=".semiautotexrc"
if [ ! -f $rcfile ]; then
    rcfile="${HOME}/.semiautotexrc"
    if [ ! -f $rcfile ]; then
	touch $rcfile
	echo '############################################################
## example of rc file for SemiAutoTeX
############################################################
latex="platex -synctex=1"
latexdraft="platex"
bibtex="pbibtex"
makeindex="mendex -U"
dvipdf="dvipdfmx"
pdfviewer="skim_reload.sh -g"' > $rcfile
	echo "SemiAutoTeX: rc file ($rcfile) is generated."
    fi
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
	    "pdfviewer" )
		PDFVIEWER="`echo ${LINE} | cut -d'\"' -f2`";;
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
	    message="typeset"
	else
	    $LATEXDRAFT $texfile && typset_pass=1
	    message="typeset(d)"
	fi
	if [ $typset_pass = 1 ]; then
	    while checksum_before="$checksum" && \
		checksum=`md5 -q $texfile.aux` && \
		[ "$checksum" != "$checksum_before" ]; do
		$LATEX $texfile 
		message=`echo "${message}+typeset"`
	    done
	fi 
	;;
    "bib" )
	$LATEXDRAFT $texfile && typset_pass=1
	message="typeset(d)"
	if [ $typset_pass = 1 ]; then
	    $BIBTEX $texfile 
	    $LATEXDRAFT $texfile && $LATEX $texfile
	    message=`echo "${message}+BibTeX+typeset(d)+typeset"`
	fi
	;;
    "idx" )
	checksum=`md5 -q $texfile.aux`
	$LATEXDRAFT $texfile && typset_pass=1
	message="typeset(d)"
	if [ $typset_pass = 1 ]; then
	    while checksum_before="$checksum" && \
		checksum=`md5 -q $texfile.aux` && \
		[ "$checksum" != "$checksum_before" ]; do
		$LATEXDRAFT $texfile 
		message=`echo "${message}+typeset(d)"`
	    done
	    $MAKEINDEX $texfile 
	    $LATEX $texfile 
	    message=`echo "${message}+MakeIndex+typeset"`
	fi
	;;
esac

if [ $typset_pass = 1 ]; then
    if [ "$DVIPDF" != "" ]; then
	$DVIPDF $texfile 
	message=`echo "${message}+DVIPDF"`
    fi
    echo "SemiAutoTeX: $message"
    if [ $pdfviewmode = 1 ]; then
	echo "$PDFVIEWER $texfile.pdf"
	$PDFVIEWER $texfile.pdf
    fi
else 
    echo "SemiAutoTeX: failed"
    exit 1
fi
