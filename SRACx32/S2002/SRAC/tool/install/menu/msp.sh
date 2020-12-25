#!/bin/csh
#
##################################################################
#
#  I am a menu command for MSP source generation. (by Keisuke OKUMURA)
#
#  Note : If this shell is not executed by @PunchMe, set the full path name
#         of the SRAC code file in the following statement, and remove the 
#         first comment indicator(#).                                      
#                                                                          
#  setenv SRAC_CODE $HOME/SRAC                                             
#
##################################################################
   set SRAC_DIR = $SRAC_CODE
#
alias echo echo
alias cd   cd   
echo " "
echo "=============================================================="
echo "SRAC source programs will be generated for the machines whose"
echo "OS type is MSP (FACOM M780,VP-2600,etc) by changing include   "
echo "statements and PDS access routines in programs.               "
echo "The generated programs will be stored in :                    " 
echo "$HOME/SRAC-MSP/.                                              " 
echo "=============================================================="
echo -n "Are you ready (y/n) ? ==> "
set conf = $<
if ($conf == "y") then
  $SRAC_DIR/tool/else/IncConv/UNIXtoMSP/change.sh
  echo " XXX Source programs for MSP were generated in $HOME/SRAC-MSP/."
else
endif
exit
