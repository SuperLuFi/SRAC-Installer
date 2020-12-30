#!/bin/csh
#
######################################################################
#
#    Generate kintab.dat(Bickley function table) for SRAC/ASMBURN
#    Load modules must be prepared before this job.
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
   setenv fu10  $SRAC_DIR/lib/kintab.dat
   set OUTLST = $SRAC_DIR/tmp/kintab.outlist
   $SRAC_DIR/tool/kintab/kintab.out >& $OUTLST
