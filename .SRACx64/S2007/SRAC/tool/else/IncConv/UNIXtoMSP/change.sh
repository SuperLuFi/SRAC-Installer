#!/bin/csh
#==============================================================
#incconv include statment conversion 96/01/05
# Usage :
# 1) UNIX ==> MSP
#         include 'filename'==> *INCLUDE filename
#   incconv -sv file > ofile
# 2) MSP  ==> UNIX
#    *INCLUDE filename      ==>      include 'filename'
#   incconv -vs file > ofile
#==============================================================
#   Generate Source Programs for FACOM-MSP
#
#  Note : If this shell is not executed by @PunchMe, set the full path name
#         of the SRAC code file in the following statement, and remove the 
#         first comment indicator(#).                                      
#                                                                          
#  setenv SRAC_CODE $HOME/SRAC                                             
#
#********************************************
    set SRAC_DIR = $SRAC_CODE
    alias rm rm
    alias cat cat
    alias mkdir
    alias cp cp
    alias cd cd
#
    set SRC_DIR = $SRAC_DIR/src
    set ODIR    = $HOME/SRAC-MSP
    if ( -e $ODIR ) then
      rm -r $ODIR
    endif
    mkdir $ODIR
    cd $SRAC_DIR/tool/else/IncConv/UNIXtoMSP
#
    cat $SRC_DIR/burn/*.f         >  $ODIR/burn.tmp
    incconv -sv  $ODIR/burn.tmp   >  $ODIR/burn.ps
    rm $ODIR/burn.tmp
#
    cat $SRC_DIR/cit/*.f          >  $ODIR/cit.tmp
    incconv -sv  $ODIR/cit.tmp    >  $ODIR/cit.ps
    rm $ODIR/cit.tmp
#
    cat $SRC_DIR/citvp/*.f        >  $ODIR/citvp.tmp
    incconv -sv  $ODIR/citvp.tmp  >  $ODIR/citvp.ps
    rm $ODIR/citvp.tmp
#
    cat $SRC_DIR/pij/*.f          >  $ODIR/pij.tmp
    incconv -sv  $ODIR/pij.tmp    >  $ODIR/pij.ps
    rm $ODIR/pij.tmp
#
    cat $SRC_DIR/srac/*.f         >  $ODIR/srac.tmp
    incconv -sv  $ODIR/srac.tmp   >  $ODIR/srac.ps
    rm $ODIR/srac.tmp
#
    cat $SRC_DIR/sracvp/*.f       >  $ODIR/sracvp.tmp
    incconv -sv  $ODIR/sracvp.tmp >  $ODIR/sracvp.ps
    rm $ODIR/sracvp.tmp
#
    cat $SRC_DIR/common/*.f       >  $ODIR/common.tmp
    incconv -sv  $ODIR/common.tmp >  $ODIR/common.ps
    rm $ODIR/common.tmp
#
    cat $SRC_DIR/extnl/facom-msp/read/*.f   >  $ODIR/read.tmp
    incconv -sv  $ODIR/read.tmp   >  $ODIR/read.ps
    rm $ODIR/read.tmp
#
    cat $SRC_DIR/extnl/facom-msp/*.f >  $ODIR/extnl.tmp
    incconv -sv  $ODIR/extnl.tmp  >  $ODIR/extnl.ps
    rm $ODIR/extnl.tmp
#
    mkdir $ODIR/PLOTMSP
    cp  $SRC_DIR/plot/*.f $ODIR/PLOTMSP
    cp  $SRC_DIR/extnl/facom-msp/plot/*.f $ODIR/PLOTMSP
    cat $ODIR/PLOTMSP/*.f         >  $ODIR/PLOTMSP/plot.tmp
    incconv -sv  $ODIR/PLOTMSP/plot.tmp   >  $ODIR/plot.ps
    rm -r $ODIR/PLOTMSP
