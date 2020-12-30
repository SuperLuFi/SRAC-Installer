#!/bin/csh
#
##################################################################
#
#  I am a menu command to customize the installer of load module
#  by changing fortran compliler option     (by Keisuke OKUMURA)
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
set menu = " "
while ($menu != "q")
  echo "================< Custom-Installation Menu >================="
  echo "*                                                           *"
  echo "*  This option should be used when you failed the installa- *"
  echo "*  tion of SRAC load module.                                *"
  echo "*                                                           *"
  echo "*  1 : Show present installation shellsrcipt                *"
  echo "*  2 : Change compiler or its options                       *"
  echo "*  3 : Install load module with the present shellscript     *"
  echo "*  4 : Make a full source file for manual installation      *"
  echo "*  h : Help (Q & A)                                         *"
  echo "*  q : bye-bye                                              *"
  echo "*                                                           *"
  echo "============================================================="
  echo -n "Set number ==> "
  set number = $<
  switch ($number)
    case 1:
          cat $SRAC_DIR/tool/lmmake/lmmk/lmmk.sh | more
          echo -n "Return>"
          set dummy  = $<
          breaksw
    case 2:
          echo -n "Is this a scalar machine ? (y/n) ==> "
          set conf = $<
          if ($conf == "n") then
            echo " "
            echo "This option is available only for scalar machines."
            echo "Edit the shellscript : tool/lmmake/lmmk/lmmk.sh,"
            echo "and try installation by yourself.  Good luck!    "
            echo " "
          else if ($conf == "y") then
            echo " "
            echo "Compile driver or options in all shellscripts in the"
            echo "dirctory : tool/lmmake/ will be replaced by input   "
            echo "characters.     "
            echo -n "Are you ready ? (y/n) ==> " 
            set conf2 = $<
            if ($conf2 == "y") then
#======================== replacement of compile option 
#
set conf3 = "n"
while ($conf3 != "y")
  echo " "
  echo -n "Enter new compile driver name (e.g. f77) ==> "
  set F77 = $<
  echo -n "Sure ? (y/n/q:quit) ==> "
  set conf3 = $<
  if ($conf3 == "q") then
    exit
  endif
end
set conf3 = "n"
while ($conf3 != "y")
  echo " "
  echo    "Enter new compile option. If many options are necessary,"
  echo -n "use (/) instead of blank delimitater (e.g. -O2/-B  )==> "
  set inp = $<
  set OPT = `echo $inp | tr / " " | cat `
  echo -n "Sure ? (y/n/q:quit) ==> "
  set conf3 = $<
  if ($conf3 == "q") then
    exit
  endif
end
#---------------------------------------------------------
cd $SRAC_DIR/tool/else/F77conv
sed -e "s/tmp1/$F77/g" < f77sh.org | sed -e "s/tmp2/$OPT/g" >! f77conv.sed
cd $SRAC_DIR/tool
#cp -r install/SysDpnd/Common/lmmake .
set cnvtr = else/F77conv/f77conv.sed
#
set Out = lmmake/lmmk/lmmk.sh
set In  = install/SysDpnd/Common/$Out
sed -f $cnvtr $In  >! $Out
#
set Out = lmmake/lmupdt/lmsizesc.sh
set In = install/SysDpnd/Common/$Out
sed -f $cnvtr $In  >! $Out
#
set Out = lmmake/lmupdt/lmupsc.sh
set In = install/SysDpnd/Common/$Out
sed -f $cnvtr $In  >! $Out
#
set Out = lmmake/objmk/cmpilsc.sh
set In = install/SysDpnd/Common/$Out
sed -f $cnvtr $In  >! $Out
#
set Out = lmmake/objmk/updtobj.sh
set In = install/SysDpnd/Common/$Out
sed -f $cnvtr $In  >! $Out
#
#======================== replacement of compile option 
            endif
          else
            echo "Invalid reply"
          endif
          echo " "
          breaksw
    case 3:
          echo -n "Do you install right now (not batch job)? (y/n)? ==> "
          set conf = $<
          if ($conf == "y") then
            echo -n "Load module instalation will start. Ready (y/n)? ==> "
            set conf2 = $<
            if ($conf2 == "y") then
              set OUTP = $SRAC_DIR/tmp/ProgInst.outlist
              rm $OUTP
              $SRAC_DIR/tool/install/ProgInst.sh >& $OUTP &
              echo " "
              echo "Wait until the background job will finish.(about 30 min.)"
              echo "Results will be written in $OUTP"
              echo " "
            else
              echo " "
            endif
          else if ($conf == "n") then
            echo " "
            echo "Use the following shellscript;"
            echo "$SRAC_DIR/tool/lmmake/lmmk/lmmk.sh"
            echo " "
          else
            echo " "
          endif
          breaksw
    case 4:
          echo -n "Is this a scalar machine ? (y/n) ==> "
          set conf = $<
          if ($conf == "n") then
            echo " "
            echo "This option is available only for scalar machines."
            echo "See the shellscript : tool/lmmake/lmmk/lmmk.sh,"
            echo "as a reference"
            echo " "
          else if ($conf == "y") then
            $SRAC_DIR/tool/install/menu/custom/source.sh 
          else
            echo "Invalid reply"
          endif
          echo " "
          breaksw
    case h:
          cat $SRAC_DIR/tool/install/menu/custom/qa.txt | more
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
