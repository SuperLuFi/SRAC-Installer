#!/bin/csh
#==============================================================
# Fortran Compile Driver Conversion ( F77   = ???  System-dependent)
#                                   ( OPT   = ???  System-dependent)
#==============================================================
#
#  Note : If this shell is not executed by @PunchMe, set the full path name
#         of the SRAC code file in the following statement, and remove the 
#         first comment indicator(#).                                      
#
#  setenv SRAC_CODE $HOME/SRAC
#
#########################################################################
    set SRAC_DIR = $SRAC_CODE
#
    set cnvtr = $SRAC_DIR/tool/else/F77conv/f77conv.sed
#
#----------- Makefile of SRAC ----------------------------
    set TGT_DIR  = $SRAC_DIR/obj/SCall
    sed -f $cnvtr $TGT_DIR/Makefile.org  >!  $TGT_DIR/Makefile
#---------------------------------------------------------
