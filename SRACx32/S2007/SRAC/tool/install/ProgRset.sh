#!/bin/csh
#
###########################################################################
#  Delete load modules and objects installed by ProgInst.sh
#                  by keisuke OKUMURA (JAERI)                                 
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
alias mkdir mkdir
#
echo "Load module and objects of SRAC installed by ProgInst.sh will be deleted."
echo -n "Sure (y/n) ? ==> "
set conf = $<
if ($conf == "y") then
  echo " "
#
#=== Objects and Load Module of SRAC95 ====================================
#
  rm -r $SRAC_DIR/obj/*
  mkdir $SRAC_DIR/obj/SCall
  mkdir $SRAC_DIR/obj/VPpart
  rm -r $SRAC_DIR/bin
  mkdir $SRAC_DIR/bin
  if (-e $SRAC_DIR/tmp/ProgInst.outlist) then
    rm $SRAC_DIR/tmp/ProgInst.outlist   
  endif
  echo " XXX Objects and load module of the SRAC code were deleted."
#
#==========================================================================
#
  echo " XXX All processes (uninstallation of load modules and objects) completed."
  echo " "
else
  exit
endif
