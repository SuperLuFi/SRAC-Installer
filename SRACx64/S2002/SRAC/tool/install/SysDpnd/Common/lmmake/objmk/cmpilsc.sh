#!/bin/csh
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
   set    SRAC_DIR = $SRAC_CODE
   set    F77      = Fortran-Driver
   set    OPTSC    = Scalar_Option 
#
   set    OBJSC    = $SRAC_DIR/obj/SCall
   set    INC      = $SRAC_DIR/src/inc/srac30m
#
   alias  rm    rm
   alias  cp    cp
   alias  cd    cd
   alias  echo  echo
#
#---------- Copy Scalar Sources and Scalar Compile
#
   cd $SRAC_DIR/src
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
