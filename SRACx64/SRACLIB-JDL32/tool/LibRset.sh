#!/bin/csh
#
#################################################################       
#                                                                       
#  Function : uninstallation of SRAC Public Libarary
#                        by Keisuke OKUMURA (JAERI)
#
   set  LIB_DIR = $SRAC_LIB_JDL32
#
#  Note : If this shell is not executed by @PunchMe, set the full path name
#         of the library file in the following statement, and use it instead 
#         of the above statement.
#
#  set  LIB_DIR = $HOME/SRACLIB-JDL32
#                                                                       
###########################################################################
#
alias rm    rm
alias cd    cd
alias ls    ls
alias grep  grep
alias echo  echo
alias mkdir mkdir
alias gzip  gzip
#
echo "Binary library data (pds/*) and temporary files (tmp/*) will be deleted."
echo -n "Sure (y/n) ? ==> "
set conf = $<
if ($conf == "y") then
  echo " XXX Uninstallation of SRAC library started."
#
#=== Compress Text Libraries ==============================================
#
  cd $LIB_DIR/txtpds
# set exst = `ls | grep txt.Z | wc -w `
  set exst = `ls | grep txt.gz | wc -w `
  if ($exst < 3) then
#   compress *.txt
    gzip     *.txt
    echo " XXX Text library data was compressed."
  endif
  set exst = `ls | grep txt | wc -w `
  if ($exst > 3) then
#   rm *.txt
#   echo " XXX Uncompressed text library data (maybe destroyed) was deleted."
    echo " XXX Text library data may be abnormal (Check it carefully !)"
  endif
#
#=== Remove PDS Libraries =================================================
#
  cd $LIB_DIR/pds
  if (-e pfast) then
    rm -r pfast
    echo " XXX Binary fast library was deleted."
  endif
  if (-e pmcrs) then
    rm -r pmcrs
    echo " XXX Binary mcross library was deleted."
  endif
  if (-e pthml) then
    rm -r pthml
    echo " XXX Binary thermal library was deleted."
  endif
#
#=== Temporary File =======================================================
#
  cd $LIB_DIR
  if ( -e tmp) then
    rm -r tmp
    mkdir tmp
  else
    mkdir tmp
  endif
  cd $LIB_DIR/tool
  if ( -e sracdir.txt) then
    rm sracdir.txt
  endif
  echo " XXX Temporary files were deleted."
#
#==========================================================================
#
  echo " XXX All processes of library uninstallation completed."
else   
  exit
endif
