#!/bin/csh -f
#
######################################################################
#
#  cmpilsc   <<   Scalar Compile & Keep Objects of SRAC code   >>
#  obj/SCall : scalar compiled full subroutines
#
#  Note : If this shell is not executed by @PunchMe, set the full path name
#         of the SRAC code file in the following statement, and remove the 
#         first comment indicator(#).                                      
#                                                                          
#  setenv SRAC_CODE $HOME/SRAC                                             
#
######################################################################
   set SRAC_DIR = $SRAC_CODE
   set    F77      = f77sx
   set    OPTSC    = "-Nv"
#  set    OPTSC    = "-Nv -O nomove"
   set    OBJSC    = $SRAC_DIR/obj/SCall
   set    SRC_DIR  = $SRAC_DIR/src
   set    INC      = $SRC_DIR/inc/srac50m
#
   alias  rm    rm
   alias  cp    cp
   alias  cd    cd
#
#---------- Copy Scalar Sources and Scalar Compile
#
   cd $SRC_DIR
   cp plot/*.f          $OBJSC
   cp extnl/*.f         $OBJSC
   cp common/*.f        $OBJSC
   cp srac/*.f          $OBJSC
   cp read/*.f          $OBJSC
   cp burn/*.f          $OBJSC
   cp cit/*.f           $OBJSC
   cp pij/*.f           $OBJSC
   cp $INC/*INC         $OBJSC
   echo "+++ all source programs are copyed into object directory +++"
#
   cd $OBJSC
   $F77 -c $OPTSC *.f
   echo "+++ end scalar compile process for all sources +++"
#
   rm *.f
   echo "+++ all process finished +++"
