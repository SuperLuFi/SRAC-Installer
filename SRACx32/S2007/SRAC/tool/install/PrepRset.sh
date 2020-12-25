#!/bin/csh
#
###########################################################################
#  Delete installed system-dependent data (sources and makefile) by PrepInst.sh
#                                         by keisuke OKUMURA (JAERI)
#
#  Note : If this shell is not executed by @PunchMe, set the full path name
#         of the SRAC code file in the following statement, and remove the 
#         first comment indicator(#).                                      
#                                                                          
#  setenv SRAC_CODE $HOME/SRAC
#
###########################################################################
   set SRAC_DIR = $SRAC_CODE
#
alias rm    rm
alias echo  echo
#
echo "System-dependent data installed by pre-processor will be deleted."
echo -n "Sure (y/n) ? ==> "
set conf = $<
if ($conf == "y") then
  echo " "
#
#=== System-dependent Source Programs =====================================
#
  set exst = `ls $SRAC_DIR/src/extnl/*.f | wc -w `
  if ($exst != 0) then
    rm -r $SRAC_DIR/src/extnl/*.f
  endif
#
  set exst = `ls $SRAC_DIR/src/extnl/*.c | wc -w `
  if ($exst != 0) then
    rm -r $SRAC_DIR/src/extnl/*.c
  endif
#
  set exst = `ls $SRAC_DIR/src/*.sx | wc -w `
  if ($exst != 0) then
    rm -r $SRAC_DIR/src/*.sx
  endif
  echo " XXX System-dependent source programs were deleted."
#
#=== System-dependent Makefile ============================================
#
  if (-e $SRAC_DIR/tool/else/mkmk/mkmk.sh) then
    rm $SRAC_DIR/tool/else/mkmk/mkmk.sh
  endif
#
  if (-e $SRAC_DIR/tool/else/F77conv/f77conv.sed) then
    rm $SRAC_DIR/tool/else/F77conv/f77conv.sed
  endif
#
  if (-e $SRAC_DIR/tool/else/F77conv/f77convS.sed) then
    rm $SRAC_DIR/tool/else/F77conv/f77convS.sed
  endif
#
  if (-e $SRAC_DIR/tool/else/F77conv/@ReadMe) then
    rm $SRAC_DIR/tool/else/F77conv/@ReadMe
  endif
#
  if (-e $SRAC_DIR/tool/else/CCconv/ccconv.sed) then
    rm $SRAC_DIR/tool/else/CCconv/ccconv.sed
  endif
#
  if (-e $SRAC_DIR/tool/else/CCconv/ccconvS.sed) then
    rm $SRAC_DIR/tool/else/CCconv/ccconvS.sed
  endif
#
  if (-e $SRAC_DIR/util/pdscnvt/src/txttopds/MakeTP) then
    rm $SRAC_DIR/util/pdscnvt/src/txttopds/MakeTP
  endif
#
  if (-e $SRAC_DIR/util/pdscnvt/src/pdstotxt/MakePT) then
    rm $SRAC_DIR/util/pdscnvt/src/pdstotxt/MakePT
  endif
#
  if (-e $SRAC_DIR/tool/kintab/Makefile) then
    rm $SRAC_DIR/tool/kintab/Makefile
  endif
#
  if (-e $SRAC_DIR/util/pdsmdl/main/BnupEdit/Makefile) then
    rm $SRAC_DIR/util/pdsmdl/main/BnupEdit/Makefile
  endif
#
  if (-e $SRAC_DIR/util/pdsmdl/main/FluxEdit/Makefile) then
    rm $SRAC_DIR/util/pdsmdl/main/FluxEdit/Makefile
  endif
#
  if (-e $SRAC_DIR/util/pdsmdl/main/FluxPlot/Makefile) then
    rm $SRAC_DIR/util/pdsmdl/main/FluxPlot/Makefile
  endif
#
  if (-e $SRAC_DIR/util/pdsmdl/main/MacroEdit/Makefile) then
    rm $SRAC_DIR/util/pdsmdl/main/MacroEdit/Makefile
  endif
#
  if (-e $SRAC_DIR/util/pdsmdl/main/MicroEdit/Makefile) then
    rm $SRAC_DIR/util/pdsmdl/main/MicroEdit/Makefile
  endif
#
  if (-e $SRAC_DIR/util/pdsmdl/main/AnisnXS/Makefile) then
    rm $SRAC_DIR/util/pdsmdl/main/AnisnXS/Makefile
  endif
#
  echo " XXX System-dependent Makefiles were deleted."
#
#=== System-dependent Shell-Script ========================================
#
  if (-e $SRAC_DIR/tool/lmmake) then
    rm -r $SRAC_DIR/tool/lmmake
  endif
  echo " XXX System-dependent shell-scripts were deleted."
#
#=== Others (for f2c+gcc/Linux or FreeBSD)================================
#
  if (-e $SRAC_DIR/tmp/check.out) then
    rm -r $SRAC_DIR/tmp/check.out
  endif
#
  if (-e $SRAC_DIR/tmp/check.tmp) then
    rm -r $SRAC_DIR/tmp/check.tmp
  endif
#
  if (-e $SRAC_DIR/cmnd/fort77) then
    rm -r $SRAC_DIR/cmnd/fort77
    echo " XXX The fort77 script in SRAC/cmnd/ was deleted."
  endif
#
  if (-e $SRAC_DIR/cmnd/f77) then
    rm -r $SRAC_DIR/cmnd/f77
    echo " XXX The f77 script in SRAC/cmnd/ was deleted."
  endif
#
#==========================================================================
#
  echo " XXX All processes (uninstallation of pre-processed data) completed."
  echo " "
else
  exit
endif
