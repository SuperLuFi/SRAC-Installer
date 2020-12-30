#!/bin/csh
#
######################################################################
#
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
   set    F77      = Fortran-Driver
   set    OPTSC    = Scalar_Option
   set    LIB      = 
#
#---------- Set Load Module Name & Directory Name of Include Statement
#
   set    LMN      = $SRAC_DIR/bin/SRAC.100m
   set    INC      = $SRAC_DIR/src/inc/srac100m
#
#---------- Make Working Directory -------------------------------
#
   set    DATE     = `date +%Y.%m.%d.%H.%M.%S`
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
   cp common/*.f        $WKDIR
   cp srac/*.f          $WKDIR
   cp read/*.f          $WKDIR
   cp burn/*.f          $WKDIR
   cp cit/*.f           $WKDIR
   cp pij/*.f           $WKDIR
   cp $INC/*            $WKDIR
#
#---------- Compile & Link Process ------------
#
   cd $WKDIR
   $F77 -o $LMN $LIB $OPTSC *.f
   echo "--- end compile process ---"
#
#---------- Remove Working Directory ----------
#
   cd ..
   rm -r $WKDIR
#
#---------- End Process -----------------------
