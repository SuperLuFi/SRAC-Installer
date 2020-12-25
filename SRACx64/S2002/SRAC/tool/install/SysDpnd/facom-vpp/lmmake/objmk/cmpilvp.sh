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
#  set    F77      = frtpx
   set    F77      = frt
   set    OPTVP    = "-Oe"
#  set    OPTVP    = "-Oe -Wv,-te -Ps"
#  set    OPTVP    = "-Oe -Wv,-te"
   set    OBJVP    = $SRAC_DIR/obj/VPpart
   set    INC      = $SRAC_DIR/src/inc/srac200m
#
   alias  rm    rm
   alias  cp    cp
   alias  cd    cd
#
#---------- Copy Vector Sources and Vector Compile
#
   cd $SRAC_DIR/src
   cp citvp/*.f     $OBJVP
   cp sracvp/*.f    $OBJVP
   cp $INC/*INC     $OBJVP
   echo "+++ vector source programs are copyed into object directory +++"
#
   cd $OBJVP
   $F77 -c $OPTVP *.f
   echo "+++ end vector compile process for vector sources +++"
#
   rm *.f
   echo "+++ all process finished +++"
