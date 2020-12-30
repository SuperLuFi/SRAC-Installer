#!/bin/csh
#
###########################################################################
#  Install SRAC load module
#                    by keisuke OKUMURA (JAERI)
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
alias echo  echo
#
#=============== Load Module of SRAC ======================================
#
echo " XXX Production of SRAC load module started."
$SRAC_DIR/tool/lmmake/lmmk.sh
echo " XXX Production of SRAC load module completed."
echo " XXX All processes completed."
#
#==========================================================================
