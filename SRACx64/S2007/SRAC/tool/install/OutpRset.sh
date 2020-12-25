#!/bin/csh
#
###########################################################################
#  Delete calculated results in SRAC/smpl/outp and temporary files in SRAC/tmp
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
alias mv    mv
alias cp    cp
alias echo  echo
alias mkdir mkdir
#
echo "All output data except original samples will be deleted in "
echo "$SRAC_DIR/smpl/outp,"
echo "$SRAC_DIR/tmp"
echo ""
echo -n "Sure (y/n) ? ==> "
set conf = $<
if ($conf == "y") then
  echo " "
#
#=== working directory of SRAC ============================================
#
  rm -r $SRAC_DIR/tmp
  mkdir $SRAC_DIR/tmp
#
#=== output data except original samples ==================================
#
  mv $SRAC_DIR/smpl/outp/*.Sample $SRAC_DIR/tmp
  rm -r $SRAC_DIR/smpl/outp/*
  mv $SRAC_DIR/tmp/*.Sample $SRAC_DIR/smpl/outp
  echo " XXX Output data except samples and working directory were cleaned."
#
#==========================================================================
#
  echo " XXX All processes (uninstallation of calculated data) completed."
  echo " "
else
  exit
endif
