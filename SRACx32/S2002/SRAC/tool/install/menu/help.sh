#!/bin/csh
#
##################################################################
#
#  I am a help command. (by keisuke OKUMURA)
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
alias cat  cat
set menu = " "
while ($menu != "q")
  echo " "
  echo "=====================< Help Menu >==========================="
  echo "*                                                           *"
  echo "*  1 : about the SRAC code system                           *"
  echo "*  2 : about file structure                                 *"
  echo "*  3 : about installation                                   *"
  echo "*  4 : about contact to consultants                         *"
  echo "*  5 : output all contents of help in file (help.txt)       *"
  echo "*  6 : show current version number                          *"
  echo "*  q : bye-bye                                              *"
  echo "*                                                           *"
  echo "============================================================="
  echo -n "Set number ==> "
  set number = $<
  switch ($number)
    case 1: 
          cat $SRAC_DIR/tool/install/help/help1.txt | more
          echo -n "Return>"
          set dummy  = $<
          breaksw
    case 2:
          cat $SRAC_DIR/tool/install/help/help2.txt | more
          echo -n "Return>"
          set dummy  = $<
          breaksw
    case 3:
          cat $SRAC_DIR/tool/install/help/help3.txt | more
          echo -n "Return>"
          set dummy  = $<
          breaksw
    case 4:
          cat $SRAC_DIR/tool/install/help/help4.txt | more
          echo -n "Return>"
          set dummy  = $<
          breaksw
    case 5:
          cat $SRAC_DIR/tool/install/help/help*.txt > $SRAC_DIR/help.txt
          echo "help.txt was saved."
          breaksw
    case 6:
          cat $SRAC_DIR/src/srac/stamp.f
          breaksw
    case q:
          set menu = "q"
          breaksw
    default:
          echo "$number is invalid number."
  endsw
end
exit
