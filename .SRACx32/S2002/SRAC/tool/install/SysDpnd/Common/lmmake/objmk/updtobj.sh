#!/bin/csh
#
######################################################################
#
#  updtobj   <<   Update Vector & Scalar Objects of SRAC code   >>
#  obj/SCall : scalor compiled full routines
#
#  Note : If this shell is not executed by @PunchMe, set the full path name
#         of the SRAC code file in the following statement, and remove the 
#         first comment indicator(#).                                      
#                                                                          
#  setenv SRAC_CODE $HOME/SRAC                                             
#
######################################################################
   set    SRAC_DIR = $SRAC_CODE
   set    F77      = f77
   set    OPT      = 
#
   set    OBJSC    = $SRAC_DIR/obj/SCall
#
   alias  rm    rm
   alias  cp    cp
   alias  cd    cd
   alias  echo  echo
#
#---------- Update Scalar Objects 
#
   cd $SRAC_DIR/src/updtsc
   cp *.f  $OBJSC
   cd $OBJSC
   $F77 -c $OPT *.f
   rm *.f
   echo "+++ Scalar Objects Updated +++"
#
   echo "+++ all process finished +++"
