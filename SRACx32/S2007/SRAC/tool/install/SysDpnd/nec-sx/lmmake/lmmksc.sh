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
   set    F77      = f77sx
   set    SRC_DIR  = $SRAC_DIR/src
   set    OPTSC    = "-Nv"
#  set    OPTSC    = "-Nv -O nomove"
   set    LIB      =
#
#---------- Set Load Module Name & Directory Name of Include Statement
#
   set    LMN      = $SRAC_DIR/bin/SRAC.100m
   set    INC      = $SRC_DIR/inc/srac100m
#
#---------- Make Working Directory -------------------------------
#
   set    DATE     = `date +%Y.%m.%d.%H.%M.%S`
   set    TMP      = tmpSRACsc.$DATE
   set    WKPAT    = $HOME
   set    WKDIR    = $WKPAT/$TMP
#
   alias  rm    rm
   alias  cp    cp
   alias  mkdir mkdir
   mkdir  $WKDIR
#
#---------- Copy Source Programs into Working Directory ----------
#
   cd $SRC_DIR
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
   cd $WKPAT
   rm -r $TMP
#
#---------- End Process -----------------------
