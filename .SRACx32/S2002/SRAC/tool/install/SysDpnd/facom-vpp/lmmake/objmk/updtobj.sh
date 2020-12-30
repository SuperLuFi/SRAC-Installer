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
#  set    F77      = frtpx
   set    F77      = frt
   set    OPTSC    = "-Oe -sc"
   set    OPTVP    = "-Oe"
#  set    OPTSC    = "-Ob -Wv,-sc"
#  set    OPTVP    = "-Oe -Wv,-te"
   set    OBJSC    = $SRAC_DIR/obj/SCall
   set    OBJVP    = $SRAC_DIR/obj/VPpart
#
   alias  rm    rm
   alias  cp    cp
   alias  cd    cd
   alias  echo  echo
#
#---------- Update Scalar Objects 
#
   cd $SRAC_DIR/src
   cp updtsc/*.f  $OBJSC
   cd $OBJSC
   $F77 -c $OPTSC *.f
   rm *.f
   echo "+++ Scalar Objects Updated +++"
#
#---------- Update Vector Objects from Vector Sources
#
   cd $SRAC_DIR/src
   cp updtvp/*.f  $OBJVP
   cd $OBJVP
   $F77 -c $OPTVP *.f
   rm *.f
   echo "+++ Partial Vector Objects Updated +++"
   echo "+++ all process finished +++"
