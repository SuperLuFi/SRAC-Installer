#!/bin/csh
#
###########################################################################
#  Delete utility programs installed by UtilInst.sh
#                          by keisuke OKUMURA (JAERI)
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
alias cd    cd 
alias echo  echo
alias mkdir mkdir
#
echo "Utility programs installed by UtilInst.sh will be deleted."
echo -n "Sure (y/n) ? ==> "
set conf = $<
if ($conf == "y") then
  echo " "
#
#=== Utility Codes(TXTtoPDS, PDStoTXT) ====================================
#
  cd $SRAC_DIR/util/pdscnvt/src/txttopds
  rm *.o
  cd $SRAC_DIR/util/pdscnvt/src/pdstotxt
  rm *.o
  cd $SRAC_DIR/util/pdscnvt/bin
  rm *
  echo " XXX Utilities (TXTtoPDS and PDStoTXT) were deleted."
#
#=== Bickley Function Table Generator =====================================
#
  cd $SRAC_DIR/tool/kintab
  rm *.o
  if (-e kintab.out) then
    rm kintab.out
  endif
  echo " XXX Bickley function table generator was deleted."
# 
#=== PDS Utility Programs(PDSMDL) =========================================
# 
  cd $SRAC_DIR/util/pdsmdl/obj
  rm *.o
  cd $SRAC_DIR/util/pdsmdl/bin
  rm *
  cd $SRAC_DIR/util/admin
  rm -r bin
  mkdir bin
  cd $SRAC_DIR/util/admin/liblist
  rm -r obj
  mkdir obj
  cd $SRAC_DIR/util/admin/pubxsec
  rm -r obj
  mkdir obj
  echo " XXX PDS Utility Programs (PDSMDL) were deleted."
# 
#==========================================================================
#
  cd $SRAC_DIR/tmp
  if (-e UtilInst.outlist) then
    rm UtilInst.outlist
  endif
  echo " XXX All processes (uninstallation of utility programs) completed."
  echo " "
else
  exit
endif
