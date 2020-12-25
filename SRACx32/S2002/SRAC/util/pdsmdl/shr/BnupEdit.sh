#!/bin/csh
#
######################################################################
#    run PDSMDL
#    main : BnupEdit : print burnup calulation results in members 
#                      ????BNUP/????DN?T     (by Keisuke OKUMURA)
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
   set LMN   = BnupEdit.out
   set ODR   = $HOME/tmp
   set CASE  = BnupEdit
   set MACRO = /home/`logname`/Sample/MACRO
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
# INPUT (2) : member name(A8), repeat until a blank card is entered
#
   cat - << END_DATA | $LM >& $OUTLST
$MACRO
SMPLBNUP
SMPLDN1T
        
END_DATA
#  rm $ODR/pdsmdl99.$CASE.$DATE
