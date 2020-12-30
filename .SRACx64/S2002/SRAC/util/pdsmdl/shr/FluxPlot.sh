#!/bin/csh
#
######################################################################
#    run PDSMDL on VPP-500/42 :
#    main : FluxPlot : prepare plot tables for neutron spectrum by R-Region
#                      or X-Region.       (by Keisuke OKUMURA)
######################################################################
#
# Fortran logical unit usage
#   6   standard output (Plot Data)
#  49   device used for PDS files (internally defined) 
#  99   text:message from PDSMDL (PDS read/write monitor)
#
#=============  Set by user ===================================
   set SRAC_DIR = $HOME/SRAC
#  LMD    : load module name
#  ODR    : directory in which output data will be stored
#  CASE   : case name which is refered as output file name
#  FLUX   : directory name of FLUX PDS(A72)
#     
   set LMN   = FluxPlot.out
   set ODR   = $HOME/tmp
   set CASE  = FluxPlot
   set FLUX  = /home/`logname`/Sample/FLUX
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
# INPUT (1) : directory name of FLUX
# INPUT (2) : member name(A8), repeat until blank card
#
   cat - << END_DATA | $LM >& $OUTLST
$FLUX
TESTA002
TESTA012
TESTA112
        
END_DATA
#  rm $ODR/pdsmdl99.$CASE.$DATE
