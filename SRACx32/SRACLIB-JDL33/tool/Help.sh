#!/bin/csh
#
##################################################################
#
#  I am SRAC-Library Help Command. (by Keisuke OKUMURA)
#
   set  LIB_DIR = $SRAC_LIB_JDL33
#
##################################################################
#
alias echo echo
alias cat  cat
set menu = " "
while ($menu != "q")
  echo " "
  echo "=====================< Help Menu >==========================="
  echo "*                                                           *"
  echo "*  1 : about this library                                   *"
  echo "*  2 : about installation of this library                   *"
  echo "*  3 : about available nuclide list                         *"
  echo "*  4 : about file structure                                 *"
  echo "*  5 : about contact to consultants                         *"
  echo "*  6 : output all contents of help in file (help.txt)       *"
  echo "*  q : bye-bye                                              *"
  echo "*                                                           *"
  echo "============================================================="
  echo -n "Set number ==> "
  set number = $<
  switch ($number)
    case 1: 
          cat $LIB_DIR/tool/help/help1.txt | more
          echo -n "Return>"
          set dummy  = $<
          breaksw
    case 2:
          cat $LIB_DIR/tool/help/help2.txt | more
          echo -n "Return>"
          set dummy  = $<
          breaksw
    case 3:
          cat $LIB_DIR/tool/help/help3.txt | more
          echo -n "Return>"
          set dummy  = $<
          breaksw
    case 4:
          cat $LIB_DIR/tool/help/help4.txt | more
          echo -n "Return>"
          set dummy  = $<
          breaksw
    case 5:
          cat $LIB_DIR/tool/help/help5.txt | more
          echo -n "Return>"
          set dummy  = $<
          breaksw
    case 6:
          cat $LIB_DIR/tool/help/help*.txt > $LIB_DIR/help.txt
          echo "help.txt was saved."
          breaksw
    case q:
          set menu = "q"
          breaksw
    default:
          echo "$number is invalid number."
  endsw
end
exit
