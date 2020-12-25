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
   set    F77      = f77sx
   set    SRC_DIR  = $SRAC_DIR/src
   set    OPTSC    = "-Nv"
#  set    OPTSC    = "-Nv -O nomove"
   set    OPTVP    = "-v"
   set    LIB      =
#
#---------- Set Load Module Name & Directory Name of Include Statement
#
   set    LMN      = $SRAC_DIR/bin/SRACvp.200m
   set    INC      = $SRC_DIR/inc/srac200m
#
#---------- Make Working Directory
#
   alias  rm    rm
   alias  cp    cp
   alias  mkdir mkdir
#
   set    DATE     = `date +%Y.%m.%d.%H.%M.%S`
   set    TMP      = tmpSRACvp.$DATE
   set    WKPAT    = $HOME 
   set    WKDIR    = $WKPAT/$TMP
   mkdir  $WKDIR
#
#---------- Copy Scalar Sources into Working Directory
#
   cd $SRC_DIR
   cp $SRC_DIR/plot/*.f          $WKDIR
   cp $SRC_DIR/extnl/*.f         $WKDIR
   cp $SRC_DIR/common/*.f        $WKDIR 
   cp $SRC_DIR/srac/*.f          $WKDIR 
   cp $SRC_DIR/read/*.f          $WKDIR 
   cp $SRC_DIR/burn/*.f          $WKDIR 
   cp $SRC_DIR/cit/*.f           $WKDIR 
   cp $SRC_DIR/pij/*.f           $WKDIR 
   cp $INC/*                     $WKDIR
#
#---------- Compile & Produce Scalar Objects
#
   cd $WKDIR
   $F77 -c $OPTSC *.f
   rm  *.f
#
#---------- Copy Vector Sources into Working Directory
#
   cd $SRC_DIR
   cp citvp.sx/*.f     $WKDIR
   cp sracvp.sx/*.f    $WKDIR
#
#---------- Compile & Overwite Vector Objects on Scalar Objects
#
   cd $WKDIR
   $F77 -c $OPTVP *.f
   rm  *.f
#
#---------- Link & Produce Vector Load Module
#
   $F77 -o $LMN $LIB *.o
#
#---------- Remove Objects if you want
#
   cd $WKPAT
   rm -r $WKDIR
#
#--------------------------------------------------------------
