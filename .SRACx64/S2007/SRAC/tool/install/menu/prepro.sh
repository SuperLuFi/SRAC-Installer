#!/bin/csh
#
##################################################################
#
#  I am a menu command of pre-processor. (by Keisuke OKUMURA)
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
echo "Pre-processor will generate system-dependent source programs,"
echo "Makefiles for utility programs, shell-scripts to generate a "
echo "load module of SRAC. "
set menu = " "
while ($menu != "q")
  echo "==================< Pre-processor Menu >====================="
  echo "*                                                           *"
  echo "*  q : bye-bye                                              *"
  echo "*  h : Help                                                 *"
  echo "*                                                           *"
  echo "*      Do you know who I am ?                               *"
  echo "*                                                           *"
  echo "*  1 : You don't know who I am, but you know I have a       *"
  echo "*      FORTRAN compiler.                                    *"
  echo "*  2 : I am Sun or it's relations.                          *"
  echo "*  3 : I am HP or it's relations.                           *"
  echo "*  4 : I am FACOM-VPP(Vector) or it's relations.            *"
  echo "*  5 : I am NEC-SX(Vector) or it's relations.               *"
  echo "*  6 : I am IBM AIX RISC SYSTEM/6000 or it's relations.     *"
  echo "*  7 : I am a Computer whose OS is Linux or FreeBSD system. *"
  echo "*  8 : I am Hitach-SR series or it's relations.             *"
  echo "*                                                           *"
  echo "============================================================="
  echo -n "Set number ==> "
  set number = $<
  switch ($number)
    case q:
          set menu = "q"
          breaksw
    case h:
          cat $SRAC_DIR/tool/install/help/help4.txt | more
          echo -n "Return>"
          set dummy  = $<
          breaksw 
    case 1:
          set yorn = "n"
          echo -n "Sure ? (y/n)=> "
          set yorn = $<
          if ($yorn == "y") then
            $SRAC_DIR/tool/install/SysDpnd/others/PrepInst.sh
            exit
          else
          endif
          breaksw
    case 2:
          set yorn = "n"
          echo -n "Sure ? (y/n)=> "
          set yorn = $<
          if ($yorn == "y") then
            $SRAC_DIR/tool/install/SysDpnd/ews-sun/PrepInst.sh
            exit
          else 
          endif
          breaksw
    case 3:
          set yorn = "n"
          echo -n "Sure ? (y/n)=> "
          set yorn = $<
          if ($yorn == "y") then
#           $SRAC_DIR/tool/install/SysDpnd/ews-hp/PrepInst.sh h9000
            $SRAC_DIR/tool/install/SysDpnd/ews-hp/PrepInst.sh
            exit
          else
          endif
          breaksw
    case 4:
          set yorn = "n"
          echo -n "Sure ? (y/n)=> "
          set yorn = $<
          if ($yorn == "y") then
            $SRAC_DIR/tool/install/SysDpnd/facom-vpp/PrepInst.sh
            exit
          else
          endif
          breaksw
    case 5:
          set yorn = "n"
          echo -n "Sure ? (y/n)=> "
          set yorn = $<
          if ($yorn == "y") then
            $SRAC_DIR/tool/install/SysDpnd/nec-sx/PrepInst.sh
            exit
          else
          endif
          breaksw
    case 6:
          set yorn = "n"
          echo -n "Sure ? (y/n)=> "
          set yorn = $<
          if ($yorn == "y") then
            $SRAC_DIR/tool/install/SysDpnd/ews-ibm/PrepInst.sh 
            exit
          else 
          endif
          breaksw
    case 7:
          set yorn = "n"
          echo -n "Sure ? (y/n)=> "
          set yorn = $<
          if ($yorn == "y") then
            $SRAC_DIR/tool/install/menu/PCmenu.sh
            exit
          else 
          endif
          breaksw
    case 8:
          set yorn = "n"
          echo -n "Sure ? (y/n)=> "
          set yorn = $<
          if ($yorn == "y") then
            $SRAC_DIR/tool/install/SysDpnd/hitachi-sr/PrepInst.sh 
            exit
          else 
          endif
          breaksw
##################################################################
    default:
          echo "$number is invalid number."
  endsw
end
exit
