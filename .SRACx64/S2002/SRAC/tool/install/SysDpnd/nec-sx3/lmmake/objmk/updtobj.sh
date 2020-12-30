#!/bin/csh -f
#
######################################################################
#
#  updtobj   <<   Update Vector & Scalar Objects of SRAC code   >>
#  obj/SCall : scalar compiled full routines
#  obj/VPpart: vector compiled vp-routines
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
   set    OPTVP    = "-v"
   set    OBJSC    = $SRAC_DIR/obj/SCall
   set    OBJVP    = $SRAC_DIR/obj/VPpart
   set    SRC_DIR  = $SRAC_DIR/src
#
   alias  rm    rm
   alias  cp    cp
   alias  cd    cd
#
#---------- Update Scalar Objects 
#
   cd $SRC_DIR
   cp updtsc/*.f  $OBJSC
   cd $OBJSC
   $F77 -c $OPTSC *.f
   rm *.f
   echo "+++ Scalar Objects Updated +++"
#
#---------- Update Vector Objects from Vector Sources
#
   cd $SRC_DIR
   cp updtvp/*.f  $OBJVP
   cd $OBJVP
   $F77 -c $OPTVP *.f
   rm *.f
   echo "+++ Partial Vector Objects Updated +++"
   echo "+++ all process finished +++"
