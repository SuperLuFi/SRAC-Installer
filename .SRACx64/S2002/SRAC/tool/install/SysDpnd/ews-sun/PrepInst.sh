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
#------------------------- for Sun Sparc ----------------------------------
   set SYSTM = ews-sun
   set F77   = f77
   set OPT   = "-O2 -dalign"
#--------------------------------------------------------------------------
#
alias cp    cp
alias cd    cd
alias echo  echo
#
#========== System-dependent Source Programs ==============================
#
echo " XXX Installation of system-dependent source programs started."
cd $SRAC_DIR/src/extnl
cp $SYSTM/*.f .
#-----------------------------------------               
#  PrepInst.sh      : Sparc FORTRAN 2.x
#  PrepInst.sh new  : Sparc FORTRAN 3.x
#if ($1 == "new") then
#  echo " (SPARC FORTRAN after Version 3.0 is assumed.)" 
#  cp $SYSTM/uioint.Spark3.0 uioint.f
#else
#  echo " (SPARC FORTRAN before Version 3.0 is assumed.)"
#endif
#-----------------------------------------               
echo " XXX Installation of system-dependent source programs completed."
#
#========== System-dependent Makefile =====================================
#
echo " XXX Installation of system-dependent Makefile started."
cd $SRAC_DIR/tool/else/F77conv
set MKOPT = " "
sed -e "s/tmp1/$F77/g" < f77mk.org | sed -e "s/tmp2/$MKOPT/g" >! f77conv.sed
change.sh
echo " XXX Installation of system-dependent Makefile completed."
#
#========== System-dependent Shell-script =================================
#
echo " XXX Installation of system-dependent shell-script started."
cd $SRAC_DIR/tool/else/F77conv
sed -e "s/tmp1/$F77/g" < f77sh.org | sed -e "s/tmp2/$OPT/g" >! f77conv.sed
cd $SRAC_DIR/tool
cp -r install/SysDpnd/Common/lmmake .
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
echo " XXX Installation of system-dependent shell-script completed."
#
#==========================================================================
#
echo " XXX All processes of pre-processor completed."
#
#==========================================================================
