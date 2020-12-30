#!/bin/csh
#
##################################################################
#
#  <<  Get Information of Public Library  >> 
#
#  Consultant : keisuke OKUMURA, 
#               E-mail okumura@mike.tokai.jaeri.go.jp
#
##################################################################
   alias cd    cd
   alias mkdir mkdir
   alias echo  echo
   alias rm    rm
   alias ls    ls
#
#  SRAC_DIR : top directory name of the SRAC code
#  LIB_DIR  : directory in which there are the SRAC PUBLIC library files
#  CASE   : case name which is refered as names of output files
#  LIB    : formal library name which will be used a comment in output
#             Characters blank or '(', ')' are not allowed in LIB.
#  PFAST  : directory name of public fast liblary
#  PTHML  : directory name of public thermal library
#  OUTD   : directory name in which output data will be stored
#           Major information will be written in $OUTD/$CASE.lst
#           Other files are debug information.
#
#  set SRAC_DIR = $HOME/SRAC
   set NOW    = `pwd`
   cd ../../..
   set SRAC_DIR = `pwd`
#-----------------------------
   set LIB_DIR  = $HOME/SRACLIB-JDL32
   set CASE   = JDL32
   set LIB    = JENDL-3.2
   set PFAST  = $LIB_DIR/pds/pfast
   set PTHML  = $LIB_DIR/pds/pthml
   set OUTD   = $SRAC_DIR/tmp
#
#
#=============  Change if you like ============================
#
   setenv  FASTP    $PFAST
   setenv  THERMALP $PTHML
   set DATE    = `date +%b%d.%H.%M.%S`
   set LM      = $SRAC_DIR/util/admin/bin/liblist.exe
   set OUTLST  = $OUTD/$CASE.FT06.$DATE
   setenv fu99   $OUTD/$CASE.FT99.$DATE
   setenv fu10   $OUTD/$CASE.lst
#
   set WKDR    = $OUTD/LIBtmp.$CASE.$DATE
   mkdir $WKDR 
   cd $PTHML
   echo "$LIB"     > $WKDR/tmp.$DATE 
   ls -1 C??????? >> $WKDR/tmp.$DATE
   echo " "       >> $WKDR/tmp.$DATE
#
#=============  Execution ========================================
#
cd $WKDR
$LM < $WKDR/tmp.$DATE >& $OUTLST 
#
#=============  Remove temporary and debug files =================
#
   cd $HOME
   rm -r $WKDR
#  rm $OUTD/$CASE.FT06.$DATE
#  rm $OUTD/$CASE.FT99.$DATE
