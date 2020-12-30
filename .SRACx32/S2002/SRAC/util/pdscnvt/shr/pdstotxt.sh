#!/bin/csh
#
######################################################################
#
#                        convert a binary-pds to a text-ps
#    PDS ===> TEXT       pdstotxt.out : load module, (make -f MakePT)  
#                        by keisuke OKUMURA (JAERI),
#                        E-mail:okumura@mike.tokai.jaeri.go.jp
#
######################################################################
   set SRAC_DIR = $HOME/SRAC
#
#---------- Set by user -----------------
#
#  TEXT : file name of the output text (?.txt)
#  DIRT : directory name in which the produced text will be stored
#  DIRP : directory name in which the pds members exist
#  DIRO : directory name in which output message will be stored
#  DIRW : work-directory name in which a member list file will be produced
#         the member list file will be removed in the last step
#
   set    TEXT = pfastj32.txt
   set    DIRT = $HOME/tmp
   set    DIRP = $HOME/SRACLIB-JDL32/pds/pfast
   set    DIRO = $HOME/tmp
   set    DIRW = $HOME/tmp
#
#---------- Change if you like ----------
#
#  OMSG : file name of output message
   set    CASE = `basename $TEXT .txt`
   set    DATE = `date +%b%d.%H.%M.%S`
   set    OMSG = PDStoTXT.$CASE.$DATE
#
#***** generate member list file *****
#
   alias  ls    ls
   alias  rm    rm
   alias  cd    cd
   cd     $DIRP
#
#  you can select members by using meta-character
   ls -1               > $DIRW/memlist.$DATE
#  ls -1  ????????     > $DIRW/memlist.$DATE
#  ls -1  ????A0?[0ZN] > $DIRW/memlist.$DATE
#  ls -1  ?PU[0-9]*    > $DIRW/memlist.$DATE
#  ls -1  ?U0[0-9]*   >> $DIRW/memlist.$DATE
#
   cd     $HOME
#
#---------- Do not change ---------------
#
   set    TXT  = $DIRT/$TEXT
   set    OUTP = $DIRO/$OMSG
   set    LM   = $SRAC_DIR/util/pdscnvt/bin/pdstotxt.out
#
   setenv fu10 $TXT
   setenv fu11 $DIRW/memlist.$DATE
#
   cat - << END_DATA | $LM >& $OUTP
$DIRP
END_DATA
   rm     $DIRW/memlist.$DATE
