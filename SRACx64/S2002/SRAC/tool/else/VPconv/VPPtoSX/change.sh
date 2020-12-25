#!/bin/csh
#==============================================================
#vcconv  vector control statment conversion   96/02/29
# Usage :
#   FACOM FORTRAN77/EX     ==>      Monte-4(nec-sx) FORTRAN77/SX
#   *VOCL *,*              ==>      *vdir *
#   vcconv   file > ofile
#==============================================================
#
#  Note : If this shell is not executed by @PunchMe, set the full path name
#         of the SRAC code file in the following statement, and remove the 
#         first comment indicator(#).                                      
#                                                                          
#  setenv SRAC_CODE $HOME/SRAC                                             
#
#==============================================================
    set SRAC_DIR = $SRAC_CODE
    alias  rm     rm
    alias  cd     cd
    alias  cat    cat
    alias  mkdir  mkdir
#
    set PWD_DIR = $SRAC_DIR/tool/else/VPconv/VPPtoSX
#
#============ citvp => citvp.sx =================================
#
    set    OUT_DIR = $SRAC_DIR/src/citvp.sx
    if ( -e $OUT_DIR ) then
      rm -r $OUT_DIR
    endif
    mkdir  $OUT_DIR
    cat    $SRAC_DIR/src/citvp/*.f        > $OUT_DIR/citvp1.tmp
    $PWD_DIR/vcconv $OUT_DIR/citvp1.tmp   > $OUT_DIR/citvp2.tmp
    cd     $OUT_DIR
    fsplit citvp2.tmp
    $SRAC_DIR/cmnd/mvsmall *.f
    rm     $OUT_DIR/*.tmp
#
#============ sracvp => sracvp.sx ===============================
#
    cd     $PWD_DIR
    set    OUT_DIR = $SRAC_DIR/src/sracvp.sx
    if ( -e $OUT_DIR ) then
      rm -r $OUT_DIR
    endif
    mkdir  $OUT_DIR
    cat    $SRAC_DIR/src/sracvp/*.f       > $OUT_DIR/sracvp1.tmp
    $PWD_DIR/vcconv $OUT_DIR/sracvp1.tmp  > $OUT_DIR/sracvp2.tmp
    cd     $OUT_DIR
    fsplit sracvp2.tmp 
    $SRAC_DIR/cmnd/mvsmall *.f
    rm     $OUT_DIR/*.tmp
#
#================================================================
