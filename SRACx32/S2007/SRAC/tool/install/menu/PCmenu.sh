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
  echo "===================== < Linux etc  > ========================"
  echo "*                                                           *"
  echo "*  q : bye-bye                                              *"
  echo "*  h : Help                                                 *"
  echo "*                                                           *"
  echo "*  Select compilers from the following menu.                *"
  echo "*                                                           *"
  echo "*  1 : g77 (FORTRAN Compiler) + gcc (C Compiler)            *"
  echo "*  2 : f2c (FORTRAN to C Translator) + gcc                  *"
  echo "*  3 : Intel FORTRAN compiler for Linux (ifort)             *"
  echo "*  4 : FACOM FORTRAN compiler for Linux (frt)               *"
  echo "*  5 : PGI   FORTRAN and C compilers for Linux (pgf77+pgcc) *"
  echo "*  6 : gfortran + gcc compilers (GNU)                       *"
  echo "*  x : other FORTRAN and C compilers (user input)           *"
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
            $SRAC_DIR/tool/install/SysDpnd/linux-intel/PrepInst.sh
            exit
          else
          endif
          breaksw
    case 4:
          set yorn = "n"
          echo -n "Sure ? (y/n)=> "
          set yorn = $<
          if ($yorn == "y") then
            $SRAC_DIR/tool/install/SysDpnd/linux-frt/PrepInst.sh
            exit
          else
          endif
          breaksw
    case 5:
          set yorn = "n"
          echo -n "Sure ? (y/n)=> "
          set yorn = $<
          if ($yorn == "y") then
            $SRAC_DIR/tool/install/SysDpnd/linux-pgi/PrepInst.sh
            exit
          else
          endif
          breaksw
    case 6:
          set yorn = "n"
          echo -n "Sure ? (y/n)=> "
          set yorn = $<
          if ($yorn == "y") then
            $SRAC_DIR/tool/install/SysDpnd/linux-gfortran/PrepInst.sh
            exit
          else
          endif
          breaksw
    case x:
          set yorn = "n"
          echo -n "Sure ? (y/n)=> "
          set yorn = $<
          if ($yorn == "y") then
            $SRAC_DIR/tool/install/SysDpnd/linux-g77/PrepInst.sh
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
