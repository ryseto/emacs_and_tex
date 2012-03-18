################################################################################
#### SemiAutoTeX ver 0.03
################################################################################
# You can find some explanations at
# http://d.hatena.ne.jp/setoryohei/20120219
# Many improvements by Kobayashi-san (OSX Workshop)
# http://macwiki.sourceforge.jp/wiki/index.php/MacOSX_WorkShop/10.7
#
# Configuration:
# You can put an rc file `.texcmdrc' in your home directory.
# If there is an rc file `texcmdrc' in current directly, it has the priority.
# ホームディレクトリに `.texcmdrc' を置く。
# もし作業ディレクトリに `texcmdrc' がある場合は、その設定が優先させる。
# Example of the rc file:
#  LATEX="pdflatex -synctex=1"
#  LATEXDRAFT="pdflatex -draftmode"
#  BIBTEX="bibtex"
#  MAKEINDEX="makeindex"
#  DVIPDF="dvipdfm"
#  PDFVIEWER="skim_reload.sh -g"
#
# DVIPDF mode:
# If DVIPDF is given, i.e. not `nil', 
# the DVIPDF program is called after a typesetting process.
# 変数 DVIPDF が設定されているとき（`nil'以外）は、
# タイプセット処理の後にそれを実行し DVI から PDF に変換する。
#
# PDFVIEWER mode:
# If PDFVIEWER is given, i.e. not "nil", 
# the previewer proglam is called at the end.
# 変数 PDFVIEWER が設定されているとき（`nil'以外）は、
# 全ての処理の後にそれを実行しプレビュアをリロードする。
#
################################################################################
#!/bin/sh

# Help
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

# Default configuration
LATEX="pdflatex -synctex=1"
LATEXDRAFT="pdflatex -draftmode"
BIBTEX="bibtex"
MAKEINDEX="makeindex"
DVIPDF=""
#DVIPDF="dvipdfm"
PDFVIEWER="skim_reload.sh -g"

# inport rc file
[ -f "${HOME}/.${TEXCMDRC}" ] && . "${HOME}/.${TEXCMDRC}" ||:
[ -f ${TEXCMDRC} ] && . ${TEXCMDRC} ||:
if [ "$LATEXDRAFT" == "" ]; then
    LATEXDRAFT="$LATEX"
fi

# コマンドラインオプションからモードを設定。
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
    [ -f $i -o -f ${i}.tex ] && INPUTFILE=$i 
done
JOBNAME=${INPUTFILE##*/}
JOBNAME=${JOBNAME%.*}

# 既存の aux ファイルのチェックサム。
# キャッシュに残っていればそれを使う。
[ -f ${MD5LOGDIR}/${JOBNAME} ] && checksum=`cat ${MD5LOGDIR}/${JOBNAME}` ||:

case "$mode" in
## LaTeX タイプセットのみ
    "tex" ) 
# aux ファイルがない場合は必ず2回以上実行する。
	if [ -f ${JOBNAME}.aux ]; then
	    $LATEX $@ || exit 1
	    message="typeset"
	else
	    $LATEXDRAFT $@ || exit 1
	    checksum=`md5 -q ${JOBNAME}.aux`
	    $LATEX $@
	    message="typeset(d)+typeset"
	fi
# cross-references を整合させるための繰り返しは
# タイプセットによって aux ファイルが変化しなくなるまで続ける。
	while checksum_before="$checksum" && \
            checksum=`md5 -q ${JOBNAME}.aux` && \
            [ "$checksum" != "$checksum_before" ]; do
            $LATEX $@
            message="${message}+typeset"
	done
	;;
## BibTeX と必要な LaTeX タイプセットを実行
    "bib" )
# BibTeX を実行するときは、typeset(d)+BibTeX+typeset(d)+typset は最低限必要。
# 参考文献リストが書類の途中に入り、それにより以降のページ番号が変わる可能性が
# あるので、最後のタイプセットは aux ファイルの変化がなくなるまで繰り返す。
	$LATEXDRAFT $@ || exit 1
	$BIBTEX ${JOBNAME}
	$LATEXDRAFT $@
	checksum=`md5 -q ${JOBNAME}.aux`
	$LATEX $@
	message="typeset(d)+BibTeX+typeset(d)+typeset"
	while checksum_before="$checksum" && \
            checksum=`md5 -q ${JOBNAME}.aux` && \
            [ "$checksum" != "$checksum_before" ]; do
            $LATEX $@
            message="${message}+typeset"
	done
	;;
## MakeIndex と必要な LaTeX タイプセットを実行
    "idx" )
# MakeIndex では索引に単語のページ番号を与える必要があるので、
# tableofcontents などによりページ番号が変わらなくなるまで最初に
# タイプセットを繰り返す。
	$LATEXDRAFT $@ || exit 1
	message="typeset(d)"
	while checksum_before="$checksum" && \
            checksum=`md5 -q ${JOBNAME}.aux` && \
            [ "$checksum" != "$checksum_before" ]; do
            $LATEXDRAFT $@
            message="${message}+typeset(d)"
	done
	$MAKEINDEX ${JOBNAME}
# 索引作成の影響でページ番号が変わる事はないと想定して後の自動繰り返しはしない。
        $LATEX $@
        message="${message}+MakeIndex+typeset"
	;;
esac

# 次回の処理の為に、チェックサムをキャッシュに残す。
echo $checksum > ${MD5LOGDIR}/${JOBNAME}

# DVIPDF が設定されている場合は DVI から PDF に変換
if [ "$DVIPDF" != "" ]; then
    $DVIPDF ${JOBNAME}
    message=`echo "${message}+DVIPDF"`
fi
echo "SemiAutoTeX: $message"

# PDFVIEWER が設定されている場合はプレビュアー起動(再読み込み)コマンドを実行
if [ "$PDFVIEWER" != "" ]; then
    echo "$PDFVIEWER ${JOBNAME}.pdf"
    $PDFVIEWER ${JOBNAME}.pdf
fi
