#!/bin/csh
#
######################################################################
#
#    For FreeBSD/g77+gcc
#    lmmk.sh : Source Programs ===> SRAC Load Module (All Scalar Version)
#    produce load module from source programs by full compilation 
#    no objects will be kept
#
#  Note : If this shell is not executed by @PunchMe, set the full path name
#         of the SRAC code file in the following statement, and remove the 
#         first comment indicator(#).                                      
#                                                                          
#  setenv SRAC_CODE $HOME/SRAC                                             
#
######################################################################
   set SRAC_DIR = $SRAC_CODE
# 
   set    F77      = g77
   set    FFLAGS   = "-lm -fno-automatic -finit-local-zero -O2 -funroll-loops"
   set    LDFLAGS  =
#  set    LDFLAGS  = -static    # required for g77 in linking ????
#
   set    CC       = gcc
   set    CFLAGS   = -DPOSIX_C
#
#---------- Set Load Module Name & Directory Name of Include Statement
#
   set    LMN      = $SRAC_DIR/bin/SRACsc.30m
   set    INC      = $SRAC_DIR/src/inc/srac30m
#
#---------- Make Working Directory -------------------------------
#
   set    DATE     = `date +%b%d.%H.%M.%S` 
   set    WKDIR    = $SRAC_DIR/tmp/tmpSRAC.$DATE
#
   alias  rm    rm
   alias  cp    cp
   alias  cd    cd
   alias  mkdir mkdir
   alias  echo  echo 
#
#---------- Copy Source Programs into Working Directory ----------
#
   mkdir  $WKDIR
   cd $SRAC_DIR/src
   cp plot/*.f          $WKDIR
   cp extnl/*.f         $WKDIR
   cp extnl/*.c         $WKDIR
   cp common/*.f        $WKDIR
   cp srac/*.f          $WKDIR
   cp read/*.f          $WKDIR
   cp burn/*.f          $WKDIR
   cp cit/*.f           $WKDIR
   cp pij/*.f           $WKDIR
   cp $INC/*            $WKDIR
#
#---------- Compile C programs ----------------
#
   cd $WKDIR
   $CC   -c $CFLAGS *.c
   echo "--- end compile process for C-programs ---"
#
   $F77  -c $FFLAGS *.f
   echo "--- end compile process for Fortran-programs ---"
#
#
#---------- Link all objects --------
#
   $F77 -o $LMN $FFLAGS *.o $LDFLAGS
   echo "--- end linking process ---"
#
#---------- Remove Working Directory ----------
#
   cd ..
   rm -r $WKDIR
#
#---------- End Process -----------------------
