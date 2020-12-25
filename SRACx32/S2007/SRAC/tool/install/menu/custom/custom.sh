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
alias rm   rm
alias mkdir mkdir
alias cat   cat
alias cp    cp
echo " "
set menu = " "
while ($menu != "q")
  echo "================< Custom-Installation Menu >================="
  echo "*                                                           *"
  echo "*  This option should be used When you failed installation  *"
  echo "*  of SRAC load module,                                     *"
  echo "*  or When you need a Makefile for SRAC installation        *"
  echo "*                                                           *"
  echo "*  1 : Show present installation shellsrcipt                *"
  echo "*  2 : Change compiler or its options                       *"
  echo "*  3 : Install load module with the present shellscript     *"
  echo "*  -------------------------------------------------------  *"
  echo "*  m : Generate a Makefile of SRAC for manual installation  *"
  echo "*  s : Make a full source file for manual installation      *"
  echo "*  h : Help (Q & A)                                         *"
  echo "*  q : bye-bye                                              *"
  echo "*                                                           *"
  echo "============================================================="
  echo -n "Set number ==> "
  set number = $<
  switch ($number)
    case 1:
          cat $SRAC_DIR/tool/lmmake/lmmk.sh | more
          echo -n "Return>"
          set dummy  = $<
          breaksw
    case 2:
          echo -n "Is this a scalar machine ? (y/n) ==> "
          set conf = $<
          if ($conf == "n") then
            echo " "
            echo "This option is available only for scalar machines."
            echo "Edit the shellscript : tool/lmmake/lmmk.sh,"
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
  echo -n "Enter new FORTRAN compile driver name (e.g. f77) ==> "
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
  echo    "Enter new FORTRAN compile option. If many options are necessary,"
  echo -n "use (/) instead of blank delimitater (e.g. -O2/-B  )==> "
  set inp = $<
  set OPT = `echo $inp | tr / " " | cat `
  echo -n "Sure ? (y/n/q:quit) ==> "
  set conf3 = $<
  if ($conf3 == "q") then
    exit
  endif
end
#--- C compiler if any
if(-e $SRAC_DIR/tool/else/CCconv/ccconv.sed) then
  set conf3 = "n"
  while ($conf3 != "y")
    echo " "
    echo -n "Enter new C compile driver name (e.g. cc) ==> "
    set CC = $<
    echo -n "Sure ? (y/n/q:quit) ==> "
    set conf3 = $<
    if ($conf3 == "q") then
      exit
    endif
  end
  set conf3 = "n"
  while ($conf3 != "y")
    echo " "
    echo    "Enter new C compile option. If many options are necessary,"
    echo -n "use (/) instead of blank delimitater (e.g. -A/-B  )==> "
    set inp = $<
    set OPTC = `echo $inp | tr / " " | cat `
    echo -n "Sure ? (y/n/q:quit) ==> "
    set conf3 = $<
    if ($conf3 == "q") then
      exit
    endif
  end
endif
#---------------------------------------------------------
if(-e $SRAC_DIR/tool/else/CCconv/ccconv.sed) then
  cp $SRAC_DIR/tool/install/SysDpnd/Common/lmmakeC/lmmk.sh  $SRAC_DIR/tool/lmmake/lmmk-tmp.sh
else
  cp $SRAC_DIR/tool/install/SysDpnd/Common/lmmake/lmmk.sh $SRAC_DIR/tool/lmmake/lmmk-tmp.sh
endif
#
cd $SRAC_DIR/tool/else/F77conv
sed -e "s/tmp1/$F77/g" < f77mk.org | sed -e "s/tmp2/$OPT/g" >! f77conv.sed
sed -e "s/tmp1/$F77/g" < f77sh.org | sed -e "s/tmp2/$OPT/g" >! f77convS.sed
cd $SRAC_DIR/tool
set cnvtr = else/F77conv/f77convS.sed
#
set Out = lmmake/lmmk.sh
set In  = lmmake/lmmk-tmp.sh
sed -f $cnvtr $In  >! $Out
rm  $SRAC_DIR/tool/lmmake/lmmk-tmp.sh
#
if( -e $SRAC_DIR/tool/else/CCconv/ccconv.sed) then
  cd $SRAC_DIR/tool/else/CCconv
  sed -e "s/tmp1/$CC/g" < ccmk.org | sed -e "s/tmp2/$OPTC/g" >! ccconv.sed
  sed -e "s/tmp1/$CC/g" < ccsh.org | sed -e "s/tmp2/$OPTC/g" >! ccconvS.sed
  cd $SRAC_DIR/tool
  cp lmmake/lmmk.sh lmmake/lmmk-tmp.sh
  set cnvtr = else/CCconv/ccconvS.sed
#
  set Out = lmmake/lmmk.sh
  set In  = lmmake/lmmk-tmp.sh
  sed -f $cnvtr $In  >! $Out
  rm  $SRAC_DIR/tool/lmmake/lmmk-tmp.sh
endif
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
            echo "$SRAC_DIR/tool/lmmake/lmmk.sh"
            echo " "
          else
            echo " "
          endif
          breaksw
    case m:
          echo -n "Did you execute pre-processor ? (y/n) ==> "
          set conf = $<
          if ($conf == "y") then
            echo -n "Is this a scalar machime or vector one ? (s/v) ==> "
            set conf2 = $<
            if ($conf2 == "s") then
              cd $SRAC_DIR/obj
              rm -r SCall
              mkdir SCall
            else if ($conf2 == "v") then
              cd $SRAC_DIR/obj
              rm -r SCall
              rm -r VPpart
              mkdir SCall
              mkdir VPpart
            else
              echo "Invalid reply"
              echo
              exit
            endif
            cp $SRAC_DIR/tool/else/F77conv/@ReadMe SCall
            cd $SRAC_DIR/tool/else/mkmk
            echo "<< Working now, Wait a moment >>"
            $SRAC_DIR/tool/else/mkmk/mkmk.sh
            cd $SRAC_DIR/tool/else/F77conv
            $SRAC_DIR/tool/else/F77conv/changeMk.sh
            if(-e $SRAC_DIR/tool/else/CCconv/ccconv.sed) then
              cd $SRAC_DIR/tool/else/CCconv
              $SRAC_DIR/tool/else/CCconv/change.sh
            endif
            echo
            if (-e $SRAC_DIR/obj/SCall/Makefile) then
              echo " "
              echo "A sample Makefile was generated in"
              echo $SRAC_DIR/obj/SCall
              echo
              echo "Change the Makefile as you like, if necessary."
              echo "After making, include files of SRAC will be copied in the above directory."
              echo "Change the parameter values in the include files if necessary, and make again."
              echo " "
            else
              echo " "
              echo "Generation of a Makefile failed !!!"
              echo " "
            endif
          else if ($conf == "n") then
            echo " "
            echo "Execute pre-processor and select a machine type similar to your machime."
            echo "Come to this menu after that."
            echo " "
          else
            echo "Invalid reply"
          endif
          echo " "
          breaksw
    case s:
          echo -n "Is this a scalar machine ? (y/n) ==> "
          set conf = $<
          if ($conf == "n") then
            echo " "
            echo "This option is available only for scalar machines."
            echo "See the shellscript : tool/lmmake/lmmk.sh,"
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
