#!/bin/csh
#
###########################################################################
#  Pre-processor to generate system-dependent source programs, makefile, shell
#  for Linux/g77+gcc                                    (by Keisuke OKUMURA)
#
#  Note : If this shell is not executed by @PunchMe, set the full path name
#         of the SRAC code file in the following statement, and remove the 
#         first comment indicator(#).                                      
#                                                                          
#  setenv SRAC_CODE $HOME/SRAC
#
###########################################################################
   set SRAC_DIR = $SRAC_CODE
#------------------------- for PC with Linux ------------------------------
   set SYSTM = pc-unix
   set SYSAD = linux-f2c
   set F77   = fort77
   set OPT   = "-Nq300 -Nn802"
   set CC    = gcc
   set OPTC  = "-DPOSIX_C"
#--------------------------------------------------------------------------
#
alias cp    cp
alias cd    cd
alias echo  echo
alias rm    rm
alias mv    mv
#
#========== Selection of Compile Driver and Option (FORTRAN)===============
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
  echo "Present Fortran compile option (blank = no option) : $OPT"
  echo -n "Is that right ? (y/n/q:quit)? ==> "
    set conf = $<
    if ($conf == "q") then
  exit
  endif
  if ($conf != "y") then
    echo    "Enter available compile option. If many options are necessary,"
    echo -n "use (/) instead of blank delimitater (e.g. -O2/-B  )==> "
    set inp = $<
    set OPT = `echo $inp | tr / " " | cat `
  endif
end
#
#========== Selection of Compile Driver and Option (C)=====================
#
set conf = "n"
while ($conf != "y")
  echo " "
  echo "Present C compile driver name : $CC"
  echo -n "Is that right ? (y/n/q:quit)? ==> "
    set conf = $<
    if ($conf == "q") then
  exit
  endif
  if ($conf != "y") then
    echo -n "Enter available C compile driver name ==> "
    set CC = $<
  endif
end
#
set conf = "n"
while ($conf != "y")
  echo " "
  echo "Present C compile option (blank = no option) : $OPTC"
  echo -n "Is that right ? (y/n/q:quit)? ==> "
    set conf = $<
    if ($conf == "q") then
  exit
  endif
  if ($conf != "y") then
    echo    "Enter available C compile option. If many options are necessary,"
    echo -n "use (/) instead of blank delimitater (e.g. -A/-B  )==> "
    set inp = $<
    set OPTC = `echo $inp | tr / " " | cat `
  endif
end
#
#========== System-dependent Source Programs ==============================
#
echo " XXX Installation of system-dependent source programs started."
cd $SRAC_DIR/src/extnl
cp $SYSTM/*.f .
cp $SYSTM/*.c .
echo " XXX Installation of system-dependent source programs completed."
#
#========== System-dependent Makefile =====================================
#
echo " XXX Installation of system-dependent Makefile started."
cd $SRAC_DIR/tool/else/F77conv
set MKOPT = "  "
sed -e "s/tmp1/$F77/g" < f77mk.org | sed -e "s/tmp2/$MKOPT/g" >! f77conv.sed
$SRAC_DIR/tool/else/F77conv/change.sh
echo " XXX Installation of system-dependent Makefile completed."
#
cp $SRAC_DIR/tool/install/SysDpnd/$SYSAD/@ReadMe $SRAC_DIR/tool/else/F77conv
cp $SRAC_DIR/tool/install/SysDpnd/Common/mkmkF2C.sh $SRAC_DIR/tool/else/mkmk/mkmk.sh
sed -e "s/tmp1/$F77/g" < f77mk.org | sed -e "s/tmp2/$OPT/g" >! f77conv.sed
sed -e "s/tmp1/$F77/g" < f77sh.org | sed -e "s/tmp2/$OPT/g" >! f77convS.sed
#
#-----
cd $SRAC_DIR/tool/else/CCconv
sed -e "s/tmp1/$CC/g" < ccmk.org | sed -e "s/tmp2/$OPTC/g" >! ccconv.sed
sed -e "s/tmp1/$CC/g" < ccsh.org | sed -e "s/tmp2/$OPTC/g" >! ccconvS.sed
#
#========== System-dependent Shell-script =================================
#
echo " XXX Installation of system-dependent shell-script started."
cd $SRAC_DIR/tool
if ( -e lmmake ) then
  rm -r lmmake
endif
cp -r install/SysDpnd/Common/lmmakeC  $SRAC_DIR/tool
mv lmmakeC lmmake
#
set cnvtr = else/F77conv/f77convS.sed
set Out = lmmake/lmmk-tmp.sh
set In  = install/SysDpnd/Common/lmmakeC/lmmk.sh
sed -f $cnvtr $In  >! $Out
#
set cnvtr = else/CCconv/ccconvS.sed
set In  = lmmake/lmmk-tmp.sh
set Out = lmmake/lmmk.sh
sed -f $cnvtr $In  >! $Out
rm -f lmmake/lmmk-tmp.sh
#
echo " XXX Installation of system-dependent shell-script completed."
#
#==========================================================================
#
echo " XXX All processes of pre-processor completed."
#
#==========================================================================
