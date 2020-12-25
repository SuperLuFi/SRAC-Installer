#!/bin/csh
#
##################################################################
#
#  I am a menu command for OS and compilers on PC/AT (by Keisuke OKUMURA)
#                                                                             
#  Note : If this shell is not executed by @PunchMe, set the full path name
#         in the following statement, and remove the 
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
set menu = " "
while ($menu != "q")
  echo "======================< PC/AT Menu > ========================"
  echo "*                                                           *"
  echo "*  q : bye-bye                                              *"
  echo "*  h : Help                                                 *"
  echo "*                                                           *"
  echo "*  Select OS and compilers from the following menu.         *"
  echo "*                                                           *"
  echo "*  1 : Linux   + g77 (FORTRAN Compiler) + gcc (C Compiler)  *"
  echo "*  2 : Linux   + f2c (FORTRAN to C Translator) + gcc        *"
  echo "*  3 : FreeBSD + g77 + gcc                                  *"
  echo "*  4 : FreeBSD + f2c + gcc                                  *"
  echo "*                                                           *"
  echo "============================================================="
  echo -n "Set number ==> "
  set number = $<
  switch ($number)
    case 1:
          set yorn = "n"
          echo -n "Sure ? (y/n)=> "
          set yorn = $<
          if ($yorn == "y") then
            $SRAC_DIR/tool/install/SysDpnd/linux-g77/PrepInst.sh
            exit
          else
          endif
          breaksw
    case 2:
          set yorn = "n"
          echo -n "Sure ? (y/n)=> "
          set yorn = $<
          if ($yorn == "y") then
            $SRAC_DIR/tool/install/SysDpnd/linux-f2c/PrepFort.sh
            exit
          else
          endif
          breaksw
    case 3:
          set yorn = "n"
          echo -n "Sure ? (y/n)=> "
          set yorn = $<
          if ($yorn == "y") then
            $SRAC_DIR/tool/install/SysDpnd/freebsd-g77/PrepInst.sh
            exit
          else
          endif
          breaksw
    case 4:
          set yorn = "n"
          echo -n "Sure ? (y/n)=> "
          set yorn = $<
          if ($yorn == "y") then
            $SRAC_DIR/tool/install/SysDpnd/freebsd-f2c/PrepFort.sh
            exit
          else
          endif
          breaksw
    case h:
          cat $SRAC_DIR/tool/install/help/help.PC1 | more
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
