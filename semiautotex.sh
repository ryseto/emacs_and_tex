################################################################################
#### SemiAutoTeX ver 0.03
################################################################################
# You can find some explanations at
# http://d.hatena.ne.jp/setoryohei/20120219
# 
# You can put an rc file '.texcmdrc' in your home directory.
# If there is an rc file 'texcmdrc' in current directly, it has the priority.
# Example of the rc file:
#  LATEX="pdflatex -synctex=1"
#  LATEXDRAFT="pdflatex -draftmode"
#  BIBTEX="bibtex"
#  MAKEINDEX="makeindex"
#  DVIPDF="dvipdfm"
#  PDFVIEWER="skim_reload.sh -g"
################################################################################
#!/bin/sh
if [ $# == 0 -o "$1" == "-h" -o "$1" == "-help"  ]; then
    echo "SemiAutoTeX 0.03:  Semi-automatic LaTeX document generation routine
Usage: semiautotex [-b] [-i] TEXFILE 
Options:
-b          run BibTeX with LaTeX
-i          run MakeIndex with LaTeX"
    exit 0
fi

TEXCMDRC=texcmdrc
MD5LOGDIR="${HOME}/Library/Caches/TeXMD5Dir"
[ -d ${MD5LOGDIR} ] || mkdir -p ${MD5LOGDIR}

LATEX="pdflatex -synctex=1"
LATEXDRAFT="pdflatex -draftmode"
BIBTEX="bibtex"
MAKEINDEX="makeindex"
#DVIPDF="dvipdfm"
DVIPDF=""
PDFVIEWER="skim_reload.sh -g"

# inport rc file
[ -f "${HOME}/.${TEXCMDRC}" ] && . "${HOME}/.${TEXCMDRC}" ||:
[ -f ${TEXCMDRC} ] && . ${TEXCMDRC} ||:
if [ "$LATEXDRAFT" == "" ]; then
    LATEXDRAFT="$LATEX"
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

for i in $@
do
    [ -f $i -o -f ${i}.tex ] && INPUTFILE=$i || OPTIONS="$OPTIONS $i"
done

JOBNAME=${INPUTFILE##*/}
JOBNAME=${JOBNAME%.*}

[ -f ${MD5LOGDIR}/${JOBNAME} ] && checksum=`cat ${MD5LOGDIR}/${JOBNAME}` ||:

case "$mode" in
    "tex" ) 
	if [ -f ${JOBNAME}.aux ]; then
	    $LATEX $@ || exit 1
	    message="typeset"
	else
	    $LATEXDRAFT $@ || exit 1
	    message="typeset(d)"
	fi

	while checksum_before="$checksum" && \
            checksum=`md5 -q ${JOBNAME}.aux` && \
            [ "$checksum" != "$checksum_before" ]; do
            $LATEX $@
            message="${message}+typeset"
	done
	;;
    "bib" )
	$LATEXDRAFT $@ || exit 1
	message="typeset(d)"
	$BIBTEX ${JOBNAME}
	$LATEXDRAFT $@ 
	$LATEX $@
	checksum=`md5 -q ${JOBNAME}.aux`
	message=`echo "${message}+BibTeX+typeset(d)+typeset"`
	;;
    "idx" )
	$LATEXDRAFT $texfile || exit 1
	message="typeset(d)"
	while checksum_before="$checksum" && \
            checksum=`md5 -q ${JOBNAME}.aux` && \
            [ "$checksum" != "$checksum_before" ]; do
            $LATEXDRAFT $@
            message="${message}+typeset(d)"
	done
	$MAKEINDEX ${JOBNAME}
	$LATEX $@ 
	checksum=`md5 -q ${JOBNAME}.aux`
	message=`echo "${message}+MakeIndex+typeset"`
	;;
esac

echo $checksum > ${MD5LOGDIR}/${JOBNAME}

if [ "$DVIPDF" != "" ]; then
    $DVIPDF ${JOBNAME}
    message=`echo "${message}+DVIPDF"`
fi
echo "SemiAutoTeX: $message"

if [ "$PDFVIEWER" != "" ]; then
    echo "$PDFVIEWER ${JOBNAME}.pdf"
    $PDFVIEWER ${JOBNAME}.pdf
fi
