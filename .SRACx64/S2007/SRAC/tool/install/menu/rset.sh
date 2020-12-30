#!/bin/csh
#
##################################################################
#
#  I am a menu command for un-installation. (by Keisuke OKUMURA)
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
alias echo  echo
alias rm    rm 
alias mkdir mkdir
echo " "
set menu = " "
while ($menu != "q")
  echo "==================< Uninstallation Menu >===================="
  echo "*                                                           *"
  echo "*  1 : Uninstallation of pre-processed data                 *"
  echo "*  2 : Uninstallation of utility programs                   *"
  echo "*  3 : Uninstallation of Bickley function table(binary)     *"
  echo "*  4 : Uninstallation of SRAC load module and object files  *"
  echo "*                                                           *"
  echo "*  5 : Uninstallation for all (initialization)              *"
  echo "*  q : bye-bye                                              *"
  echo "*                                                           *"
  echo "============================================================="
  echo -n "Set number ==> "
  set number = $<
  switch ($number)
    case 1:
          $SRAC_DIR/tool/install/PrepRset.sh
          breaksw
    case 2:
          $SRAC_DIR/tool/install/UtilRset.sh
          breaksw
    case 3:
          $SRAC_DIR/tool/install/DataRset.sh
          breaksw
    case 4:
          $SRAC_DIR/tool/install/ProgRset.sh
          breaksw
    case 5:
          $SRAC_DIR/tool/install/ProgRset.sh
          $SRAC_DIR/tool/install/DataRset.sh
          $SRAC_DIR/tool/install/UtilRset.sh
          $SRAC_DIR/tool/install/PrepRset.sh
          $SRAC_DIR/tool/install/OutpRset.sh
          breaksw
    case q:
          set menu = "q"
          breaksw
    default:
          echo "$number is invalid number."
  endsw
end
exit
