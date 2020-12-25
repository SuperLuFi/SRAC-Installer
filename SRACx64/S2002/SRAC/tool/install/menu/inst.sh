#!/bin/csh
#
##################################################################
#
#  I am a menu command for installation. (by Keisuke OKUMURA)
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
echo "============================================================="
echo "1)Utility programs, 2)Bickley function table, 3)SRAC load     "
echo "module, can be installed here. Before these installation, the "
echo "pre-processor must be executed. The utility programs must be  "
echo "installed before the Bickley function table.                  "
echo "If SRAC load module can not be installed well, go to the      "
echo "custom-installation menu.                                     "
echo " "
set menu = " "
while ($menu != "q")
  echo "===================< Installation Menu >====================="
  echo "*                                                           *"
  echo "*  1 : Installation of utility programs                     *"
  echo "*  2 : Installation of Bickley function table (binary)      *"
  echo "*  3 : Installation of SRAC load module                     *"
  echo "*                                                           *"
  echo "*  c : Menu for custom-installation of SRAC load module     *"
  echo "*  h : Help                                                 *"
  echo "*  q : bye-bye                                              *"
  echo "*                                                           *"
  echo "============================================================="
  echo -n "Set number ==> "
  set number = $<
  switch ($number)
    case 1:
          echo -n "Sure (y/n)? ==> "
          set conf = $<
          if ($conf == "y") then
            echo " "
            echo "Wait for a short while."
            set OUTP = $SRAC_DIR/tmp/UtilInst.outlist
            $SRAC_DIR/tool/install/UtilInst.sh >& $OUTP
            echo "Results were written in $OUTP"
            echo " "
          else
          endif
          breaksw
    case 2:
          echo -n "Do you install right now (not batch job)? (y/n)? ==> "
          set conf = $<
          if ($conf == "y") then
            echo -n "Bickley function table instalation will start. Ready (y/n)? ==> "
            set conf2 = $<
            if ($conf2 == "y") then
              set OUTP = $SRAC_DIR/tmp/DataInst.outlist
              $SRAC_DIR/tool/install/DataInst.sh >& $OUTP &
              echo " "
              echo "Wait for a short while.(background job)"
              echo "Results will be written in $OUTP"
              echo " "
            else
              echo " "
            endif
          else if ($conf == "n") then
            echo " "
            echo "Edit the following shellscript as you like, and submit it."
            echo "Sellscript:$SRAC_DIR/tool/install/DataInst.sh"
            echo " "
          else
            echo " "
          endif
          breaksw
    case 3:
          echo -n "Do you install right now (not batch job)? (y/n)? ==> "
          set conf = $<
          if ($conf == "y") then
            echo -n "Load module instalation will start. Ready (y/n)? ==> "
            set conf2 = $<
            if ($conf2 == "y") then
              set OUTP = $SRAC_DIR/tmp/ProgInst.outlist
              $SRAC_DIR/tool/install/ProgInst.sh >& $OUTP &
              echo " "
              echo "Wait until the background job will finish.(about 5-30 min.)"
              echo "Results will be written in $OUTP"
              echo " "
            else
              echo " "
            endif
          else if ($conf == "n") then
            echo " "
            echo "Edit the following shellscript as you like, and submit it."
            echo "Sellscript:$SRAC_DIR/tool/install/ProgInst.sh"
            echo " "
          else
            echo " "
          endif
          breaksw
    case c:
          $SRAC_DIR/tool/install/menu/custom/custom.sh
          breaksw
    case h:
          cat $SRAC_DIR/tool/install/help/help3.txt | more
          echo -n "Return>"
          set dummy  = $<
          breaksw
    case q:
          set menu = "q"
          breaksw
    default:
          echo "$number is invalid number."
  endsw
end
exit
