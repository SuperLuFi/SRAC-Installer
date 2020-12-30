#!/bin/csh
#
#################################################################
#
#  Function : installation of SRAC Public Libarary 
#                        by Keisuke OKUMURA (JAERI)
#
   set  LIBNM = j32
#---------------------------------------------------------------
   set  LIB_DIR  = $SRAC_LIB_JDL32
#                                                                           
#  Note : If this shell is not executed by @PunchMe, set the full path names
#         of this library in the following statements and use it instead of 
#         the above statement.
#                                                                           
#  set  LIB_DIR  = $HOME/SRACLIB-JDL32
#
#################################################################
#
alias  mkdir mkdir
alias  echo  echo
alias  rm    rm
alias  cat   cat
alias  gzip  gzip
#
if( ! -e $LIB_DIR/tool/sracdir.txt) then
  echo " XXX File (tool/sracdir.txt) was not found."
  echo ' XXX Execute Installer (@PunchMe) and try again.'
  exit
endif
set    SRAC_DIR = `cat $LIB_DIR/tool/sracdir.txt`
set    LM       = $SRAC_DIR/util/pdscnvt/bin/txttopds.out
set    DATE     = `date +%Y.%m.%d.%H.%M.%S`
#
######################## FAST LIBRARY ###########################
#
set    CASE  = pfast
echo " XXX Installation of public fast library started."
set    TXT   = $LIB_DIR/txtpds/$CASE$LIBNM.txt
set    DIRP  = $LIB_DIR/pds/$CASE
set    OUTP  = $LIB_DIR/tmp/plibmk.$CASE$LIBNM.$DATE
if (-e $DIRP) then
  rm -r $DIRP
endif
mkdir  $DIRP
#uncompress $TXT.Z
gzip -d $TXT.gz
#if (-e $TXT.Z) then
if (-e $TXT.gz) then
  echo " XXX Uncompress failed, because of maybe shortage of disk space."
  echo " XXX Enlarge available disk space and try again."
  rm $TXT
  exit
endif
#  convert a text-pds library to a binary-pds library
setenv fu10 $TXT
cat - << END_DATA | $LM >& $OUTP
$DIRP
END_DATA
#compress   $TXT
gzip $TXT
echo " XXX Installation of public fast library completed."
#
######################## MCROSS LIBRARY ###########################
#
set    CASE  = pmcrs
echo " XXX Installation of public mcross library started."
set    TXT   = $LIB_DIR/txtpds/$CASE$LIBNM.txt
set    DIRP  = $LIB_DIR/pds/$CASE
set    OUTP  = $LIB_DIR/tmp/plibmk.$CASE$LIBNM.$DATE
mkdir  $DIRP
#uncompress $TXT.Z
gzip -d $TXT.gz
#if (-e $TXT.Z) then
if (-e $TXT.gz) then
  echo " XXX Uncompress failed, because of maybe shortage of disk space."
  echo " XXX Enlarge available disk space and try again."
  rm $TXT
  exit
endif
#  convert a text-pds library to a binary-pds library
setenv fu10 $TXT
cat - << END_DATA | $LM >& $OUTP
$DIRP
END_DATA
#compress   $TXT
gzip $TXT
echo " XXX Installation of public mcross library completed."
#
######################## THERMAL LIBRARY ##########################
#
set    CASE  = pthml
echo " XXX Installation of public thermal library started."
set    TXT   = $LIB_DIR/txtpds/$CASE$LIBNM.txt
set    DIRP  = $LIB_DIR/pds/$CASE
set    OUTP  = $LIB_DIR/tmp/plibmk.$CASE$LIBNM.$DATE
mkdir  $DIRP
#uncompress $TXT.Z
gzip -d $TXT.gz
#if (-e $TXT.Z) then
if (-e $TXT.gz) then
  echo " XXX Uncompress failed, because of maybe shortage of disk space."
  echo " XXX Enlarge available disk space and try again."
  rm $TXT
  exit
endif
#  convert a text-pds library to a binary-pds library
setenv fu10 $TXT
cat - << END_DATA | $LM >& $OUTP
$DIRP
END_DATA
#compress   $TXT
gzip $TXT
echo " XXX Installation of public thermal library completed."
#
###################################################################
echo
echo " XXX All processes of library ($LIBNM) installation completed."
echo " XXX Confirm output messages written in $LIB_DIR/tmp/plibmk.*." 
