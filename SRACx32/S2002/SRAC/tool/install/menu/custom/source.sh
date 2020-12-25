#!/bin/csh
#
##################################################################
#
#  I am a menu command for SRAC full-source generation. (by Keisuke OKUMURA)
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
alias echo   echo
alias cd     cd   
alias cp     cp   
alias mkdir  mkdir
echo " "
echo "=============================================================="
echo "Full-source programs will be generated for manual installation."
echo "The generated programs will be stored in :                    "
echo "$HOME/SRAC-SOURCE/.                                           "
echo "Following source programs are system-dependent:               "
echo "uclckm.f   uioset.f  (change them by yourself, if necessary)  "
echo "=============================================================="
echo -n "Are you ready (y/n) ? ==> "
set conf = $<
if ($conf == "y") then
  set ODIR = $HOME/SRAC-SOURCE
  if (-e $ODIR) then
    rm -r $ODIR
  endif
  mkdir $ODIR
  cd $SRAC_DIR/src
  cp plot/*.f          $ODIR
  cp extnl/others/*.f  $ODIR
  cp common/*.f        $ODIR
  cp srac/*.f          $ODIR
  cp read/*.f          $ODIR
  cp burn/*.f          $ODIR
  cp cit/*.f           $ODIR
  cp pij/*.f           $ODIR
  cp inc/srac30m/*INC  $ODIR
  echo " XXX Full source programs of SRAC were generated in $ODIR."
else
endif
exit
