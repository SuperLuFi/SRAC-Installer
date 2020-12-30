#!/bin/csh
#==============================================================
# C Compile Driver Conversion ( CC    = ???  System-dependent)
#                             ( OPT   = ???  System-dependent)
#==============================================================
#
#  Note : If this shell is not executed by @PunchMe, set the full path name
#         of the MOSRA file in the following statement, and remove the 
#         first comment indicator(#).
#
#  setenv SRAC_CODE $HOME/SRAC
#
#########################################################################
   set SRAC_DIR = $SRAC_CODE
#
#----------- make Makefile for SRAC ---------------------
#                                                         
    alias  mv  mv
    set cnvtr = $SRAC_DIR/tool/else/CCconv/ccconv.sed
    set TGT_DIR  = $SRAC_DIR/obj/SCall
    mv $TGT_DIR/Makefile $TGT_DIR/Makefile.org
    sed -f $cnvtr $TGT_DIR/Makefile.org  >!  $TGT_DIR/Makefile
#
#---------------------------------------------------------
