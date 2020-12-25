#!/bin/csh
#
######################################################################
#    run PDSMDL 
#    main : MacroEdit : print macro xs in the 99th device (by Keisuke OKUMURA)
######################################################################
#
# Fortran logical unit usage
#   6   standard output
#  49   device used for PDS files (internally defined) 
#  99   text:message from PDSMDL (PDS read/write monitor)
#
#=============  Set by user ===================================
   set SRAC_DIR = $HOME/SRAC
#  LMD    : load module name
#  ODR    : directory in which output data will be stored
#  CASE   : case name which is refered as output file name
#  MACRO  : directory name of MACRO/MACROWRK PDS(A72)
#     
   set LMN   = MacroEdit.out
   set ODR   = $HOME/tmp
   set CASE  = MacroEdit
   set MACRO = /home/`logname`/Sample/MACROWRK
#  
#=============  Change if you like ============================
#
   set LM    = $SRAC_DIR/util/pdsmdl/bin/$LMN
   set DATE  = `date +%b%d.%H.%M.%S`
   setenv  fu99  $ODR/pdsmdl99.$CASE.$DATE
   set OUTLST =  $ODR/pdsmdl06.$CASE.$DATE
#
#=============  Exec user's PDSMDL code =======================
# 
# INPUT (1) : directory name of MACRO/MACROWRK
# INPUT (2) : member name(A8), repeat until blank card
#
   cat - << END_DATA | $LM >& $OUTLST
$MACRO
FUELF012
FUELF01Y
        
END_DATA
#  rm $ODR/pdsmdl99.$CASE.$DATE
