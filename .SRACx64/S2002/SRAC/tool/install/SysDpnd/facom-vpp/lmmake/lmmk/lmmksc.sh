#!/bin/csh -f
#
######################################################################
#
#    lmmksc.sh : Source Programs ===> SRAC Load Module (All Scalar Version)
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
   set    LIB      =
#  set    F77      = frtpx
#  set    OPTSC    = "-Ob -Wv,-sc"
#
#---------- Set Load Module Name & Directory Name of Include Statement
#
   set    LMN      = $SRAC_DIR/bin/SRACsc.200m
   set    INC      = $SRAC_DIR/src/inc/srac200m
#
#---------- Make Working Directory -------------------------------
#
   set    DATE     = `date +%b%d.%H.%M.%S` 
   set    WKDIR    = $SRAC_DIR/tmp/tmpSRACsc.$DATE
#
   alias  rm    rm
   alias  cp    cp
   alias  cd    cd
   alias  mkdir mkdir
#
#---------- Copy Source Programs into Working Directory ----------
#
   mkdir  $WKDIR
   cd $SRAC_DIR/src
   cp plot/*.f        $WKDIR
   cp extnl/*.f       $WKDIR
   cp common/*.f      $WKDIR 
   cp srac/*.f        $WKDIR 
   cp read/*.f        $WKDIR 
   cp burn/*.f        $WKDIR 
   cp cit/*.f         $WKDIR 
   cp pij/*.f         $WKDIR 
   cp $INC/*          $WKDIR
#
#---------- Compile & Link Process ------------
#
   cd $WKDIR
   $F77 -o $LMN $LIB $OPTSC *.f
#
#---------- Remove Working Directory ----------
#
   cd ..
   rm -r $WKDIR
#
#---------- End Process -----------------------
