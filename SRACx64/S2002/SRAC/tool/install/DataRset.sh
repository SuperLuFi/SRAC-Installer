#!/bin/csh
#
###########################################################################
#  Uninstall Bickley function table installed by DataInst.sh
#
#  Note : If this shell is not executed by @PunchMe, set the full path name
#         of the SRAC code file in the following statement, and remove the 
#         first comment indicator(#).                                      
#                                                                          
#  setenv SRAC_CODE $HOME/SRAC
#
###########################################################################
   set SRAC_DIR = $SRAC_CODE
#
alias rm    rm 
alias echo  echo
#
echo "Bickley function table installed by DataInst.sh will be deleted."
echo -n "Sure (y/n) ? ==> "
set conf = $<
if ($conf == "y") then
  echo " "
#
#=== Bickley Function Table ===============================================
#
  if (-e $SRAC_DIR/lib/kintab.dat) then
    rm $SRAC_DIR/lib/kintab.dat
  endif
  echo " XXX Bickley function table was deleted."
#  
#=== Temporary File =======================================================
#
  if (-e $SRAC_DIR/tmp/DataInst.outlist) then
    rm $SRAC_DIR/tmp/DataInst.outlist
  endif
  if (-e $SRAC_DIR/tmp/kintab.outlist) then
    rm $SRAC_DIR/tmp/kintab.outlist
  endif
  echo " XXX Temporary files were deleted."
#
#==========================================================================
#
  echo " XXX All processes (uninstallation of Bickley function table) completed."
  echo " "
else   
  exit
endif
