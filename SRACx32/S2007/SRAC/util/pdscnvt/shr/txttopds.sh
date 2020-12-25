#!/bin/csh
#
######################################################################
#
#                        convert a text-ps to a binary-pds
#    TEXT ===> PDS       txttopds.out : load module, (make -f MakeTP)
#                        by keisuke OKUMURA (JAERI),
#                        E-mail:okumura@mike.tokai.jaeri.go.jp
#
#---------- Set by user -----------------
   set SRAC_DIR = $HOME/SRAC
#
#  TEXT : file name of the input text-ps (?.txt)
#  DIRT : directory name in which the text file exists
#  DIRP : directory name in which the produced pds members will be stored
#  DIRO : directory name in which output message will be stored
   set    TEXT = macro.txt
   set    DIRT = $HOME/tmp
   set    DIRP = $HOME/tmp/MacroPDS
   set    DIRO = $HOME/tmp
   mkdir  $DIRP
#
#---------- Change if you like ----------
#
#  OMSG : file name of output message
   set    CASE = `basename $TEXT .txt`
   set    DATE = `date +%b%d.%H.%M.%S`
   set    OMSG = TXTtoPDS.$CASE.$DATE
#
#---------- Do not change ---------------
#
   set    TXT  = $DIRT/$TEXT
   set    OUTP = $DIRO/$OMSG
   set    LM   = $SRAC_DIR/util/pdscnvt/bin/txttopds.out
#
   setenv fu10 $TXT
#
   cat - << END_DATA | $LM >& $OUTP
$DIRP
END_DATA
