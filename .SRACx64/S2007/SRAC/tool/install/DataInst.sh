#!/bin/csh
#
###########################################################################
#  Installation of the Bickley function table (binary) which is used in the
#  collision probability calculation in SRAC.
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
#=== Bickley Function Table(kintab.dat) ===================================
#
echo " XXX Production of Bickley function table started."
$SRAC_DIR/tool/kintab/kintab.sh
echo " XXX Production of Bickley function table completed."
#
#==========================================================================
#
echo " XXX All processes completed."
echo " XXX Confirm output messages written in $SRAC_DIR/tmp/kintab.outlist."
#
#==========================================================================
