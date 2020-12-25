#!/bin/csh
#
###########################################################################
#  Pre-processor to generate system-dependent source programs, makefile, shell
#  (by keisuke OKUMURA)
#
#  Note : If this shell is not executed by @PunchMe, set the full path name
#         of the SRAC code file in the following statement, and remove the 
#         first comment indicator(#).                                      
#                                                                          
#  setenv SRAC_CODE $HOME/SRAC
#
###########################################################################
   set SRAC_DIR = $SRAC_CODE
#------------------------- for FACOM/VPP ----------------------------------
   set SYSTM = facom-vpp
   set SYSAD = facom-vpp
#  set F77   = frtpx
   set F77   = frt
   set OPTS  = "-Oe -sc"
   set OPTV  = "-Oe"
#--------------------------------------------------------------------------
#
alias cp    cp
alias cd    cd
alias echo  echo
#
#========== Selection of Compile Driver and Option ========================
#
set conf = "n"
while ($conf != "y")
  echo " "
  echo "Present Fortran compile driver name : $F77"
  echo -n "Is that right ? (y/n/q:quit)? ==> "
    set conf = $<
    if ($conf == "q") then
  exit
  endif
  if ($conf != "y") then
    echo -n "Enter available compile driver name ==> "
    set F77 = $<
  endif
end
#
set conf = "n"
while ($conf != "y")
  echo " "
  echo "Present scalar mode Fortran compile option (blank = no option) : $OPTS"
  echo -n "Is that right ? (y/n/q:quit)? ==> "
    set conf = $<
    if ($conf == "q") then
  exit
  endif
  if ($conf != "y") then
    echo    "Enter available scalar compile option. If many options are necessary,"
    echo -n "use (/) instead of blank delimitater (e.g. -O2/-B  )==> "
    set inp = $<
    set OPTS = `echo $inp | tr / " " | cat `
  endif
end
#
set conf = "n"
while ($conf != "y")
  echo " "
  echo "Present vector mode Fortran compile option (blank = no option) : $OPTV"
  echo -n "Is that right ? (y/n/q:quit)? ==> "
    set conf = $<
    if ($conf == "q") then
  exit
  endif
  if ($conf != "y") then
    echo    "Enter available vector compile option. If many options are necessary,"
    echo -n "use (/) instead of blank delimitater (e.g. -O2/-B  )==> "
    set inp = $<
    set OPTV = `echo $inp | tr / " " | cat `
  endif
end
#
#========== System-dependent Source Programs ==============================
#
echo " XXX Installation of system-dependent source programs started."
cd $SRAC_DIR/src/extnl
cp $SYSTM/*.f .
echo " XXX Installation of system-dependent source programs completed."
#
#========== System-dependent Makefile =====================================
#
echo " XXX Installation of system-dependent Makefile started."
cd $SRAC_DIR/tool/else/F77conv
set MKOPT = " "
sed -e "s/tmp1/$F77/g" < f77mk.org | sed -e "s/tmp2/$MKOPT/g" >! f77conv.sed
$SRAC_DIR/tool/else/F77conv/change.sh
echo " XXX Installation of system-dependent Makefile completed."
#
cp $SRAC_DIR/tool/install/SysDpnd/$SYSAD/@ReadMe $SRAC_DIR/tool/else/F77conv
cp $SRAC_DIR/tool/install/SysDpnd/Common/mkmkVP.sh $SRAC_DIR/tool/else/mkmk/mkmk.sh
sed -e "s/tmp1/$F77/g" < f77mk.org | sed -e "s/tmp2/$OPTS/g" | sed -e "s/tmp3/$OPTV/g" >! f77conv.sed
sed -e "s/tmp1/$F77/g" < f77sh.org | sed -e "s/tmp2/$OPTS/g" | sed -e "s/tmp3/$OPTV/g" >! f77convS.sed
#
#========== System-dependent Shell-script =================================
#
echo " XXX Installation of system-dependent shell-script started."
cd $SRAC_DIR/tool
cp -r install/SysDpnd/$SYSTM/lmmake .
echo " XXX Installation of system-dependent shell-script completed."
#
#==========================================================================
#
echo " XXX All processes of pre-processor completed."
#
#==========================================================================
