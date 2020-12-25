#!/bin/csh -f
#
######################################################################
#
#  cmpilvp   <<   Vector Compile & Keep Objects of SRAC code   >>
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
   set    OPTVP    = "-v"
   set    OBJVP    = $SRAC_DIR/obj/VPpart
   set    SRC_DIR  = $SRAC_DIR/src
   set    INC      = $SRC_DIR/inc/srac50m
#
   alias  rm    rm
   alias  cp    cp
   alias  cd    cd
#
#---------- Copy Vector Sources and Vector Compile
#
   cd $SRC_DIR
   cp citvp,sx/*.f   $OBJVP
   cp sracvp.sx/*.f  $OBJVP
   cp $INC/*INC      $OBJVP
   echo "+++ vector source programs are copyed into object directory +++"
#
   cd $OBJVP
   $F77 -c $OPTVP *.f
   echo "+++ end vector compile process for vector sources +++"
#
   rm *.f
   echo "+++ all process finished +++"
