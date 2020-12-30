#!/bin/csh -f
#
######################################################################
#
#    lmmk.sh : Source Programs ===> SRAC Load Module (Partial VP Version)
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
#  set    F77      = frtpx
   set    F77      = frt
   set    OPTSC    = "-Oe -sc"
   set    OPTVP    = "-Oe"
   set    LIB      =
#  set    F77      = frtpx
#  set    OPTSC    = "-Ob -Wv,-sc"
#  set    OPTVP    = "-Oe -Wv,-te"
#
#---------- Set Load Module Name & Directory Name of Include Statement
#
   set    LMN      = $SRAC_DIR/bin/SRACvp.200m
   set    INC      = $SRAC_DIR/src/inc/srac200m
#
#---------- Make Working Directory
#
   alias  rm    rm
   alias  cp    cp
   alias  cd    cd
   alias  mkdir mkdir
#
   set    DATE     = `date +%b%d.%H.%M.%S`
   set    WKDIR    = $SRAC_DIR/tmp/tmpSRACvp.$DATE
#
#---------- Copy Scalar Sources into Working Directory
#
   mkdir  $WKDIR
   cd $SRAC_DIR/src
   cp plot/*.f         $WKDIR
   cp extnl/*.f        $WKDIR
   cp common/*.f       $WKDIR 
   cp srac/*.f         $WKDIR 
   cp read/*.f         $WKDIR 
   cp burn/*.f         $WKDIR 
   cp cit/*.f          $WKDIR 
   cp pij/*.f          $WKDIR 
   cp $INC/*           $WKDIR
#
#---------- Compile & Produce Scalar Objects
#
   cd $WKDIR
   $F77 -c $OPTSC *.f
   rm  *.f
#
#---------- Copy Vector Sources into Working Directory
#
   cp $SRAC_DIR/src/citvp/*.f    .
   cp $SRAC_DIR/src/sracvp/*.f   .
#
#---------- Compile & Overwite Vector Objects on Scalar Objects
#
   $F77 -c $OPTVP *.f
   rm  *.f
#
#---------- Link & Produce Vector Load Module
#
   $F77 -o $LMN $LIB *.o
#
#---------- Remove Objects if you want
#
   cd ..
   rm -r $WKDIR
#
#--------------------------------------------------------------
