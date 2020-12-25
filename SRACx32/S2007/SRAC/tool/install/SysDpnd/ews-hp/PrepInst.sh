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
#------------------------- for HP-9000 ------------------------------------
   set SYSTM = ews-hp
   set SYSAD = ews-hp
   set F77   = f77
if ($1 == "h9000") then
######### HP-9000 or its relations ###############
#  set OPT   = "-K +ppu +A3 +U77 +E1 +E4 +E7"
#  set OPT   = "-K +ppu +U77 +E1 +E4 +E7"
   set OPT   = "-K +ppu +U77 +E1 +E4 +O2"
else
######### HP-C200 or its relations ###############
   set OPT   = "-K +ppu +U77 +E1 +E4 +O2 +DA1.1"
endif
#--------------------------------------------------------------------------
#
alias cp    cp
alias cd    cd
alias echo  echo
#
#========== Selection of Compile Driver and Option ========================
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
cp $SRAC_DIR/tool/install/SysDpnd/$SYSAD/mkmkHP.sh $SRAC_DIR/tool/else/mkmk/mkmk.sh
sed -e "s/tmp1/$F77/g" < f77mk.org | sed -e "s/tmp2/$OPT/g" >! f77conv.sed
sed -e "s/tmp1/$F77/g" < f77sh.org | sed -e "s/tmp2/$OPT/g" >! f77convS.sed
#
#========== System-dependent Shell-script =================================
#
echo " XXX Installation of system-dependent shell-script started."
cd $SRAC_DIR/tool
cp -r install/SysDpnd/Common/lmmake .
set cnvtr = else/F77conv/f77convS.sed
#
set Out = lmmake/lmmk.sh
set In  = install/SysDpnd/Common/$Out
sed -f $cnvtr $In  >! $Out
#
echo " XXX Installation of system-dependent shell-script completed."
#
#==========================================================================
#
echo " XXX All processes of pre-processor completed."
#
#==========================================================================
