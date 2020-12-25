#!/bin/csh
#
###########################################################################
#  Install utility programs
#                  by keisuke OKUMURA (JAERI)
#
#  Note : If this shell is not executed by @PunchMe, set the full path name
#         of the SRAC code file in the following statement, and remove the 
#         first comment indicator(#).                                      
#
#  setenv SRAC_CODE $HOME/SRAC
#
###########################################################################   
   set SRAC_DIR = $SRAC_CODE
#
alias cd    cd
alias echo  echo 
#
#=== Utility Codes(TXTtoPDS, PDStoTXT) ====================================
#
echo " XXX Installation of TXTtoPDS and PDStoTXT started."
cd $SRAC_DIR/util/pdscnvt/src/txttopds
make -f MakeTP
cd $SRAC_DIR/util/pdscnvt/src/pdstotxt
make -f MakePT
echo " XXX Utilitie (TXTtoPDS and PDStoTXT) were installed."
#
#=== Bickley Function Table Generator =====================================
#
echo " XXX Installation of Bickley function table generator started."
cd $SRAC_DIR/tool/kintab
make
echo " XXX Installation of Bickley function table generator completed."
#
#=== PDS Utility Programs(PDSMDL) =========================================
#
echo " XXX Installation of PDS utility programs (PDSMDL) started."
cd $SRAC_DIR/util/pdsmdl/main/BnupEdit
make
cd $SRAC_DIR/util/pdsmdl/main/FluxEdit
make
cd $SRAC_DIR/util/pdsmdl/main/FluxPlot
make
cd $SRAC_DIR/util/pdsmdl/main/MacroEdit
make
cd $SRAC_DIR/util/pdsmdl/main/MicroEdit
make
cd $SRAC_DIR/util/pdsmdl/main/AnisnXS
make
echo " XXX Installation of PDS utility programs (PDSMDL) completed."
#
#==========================================================================
#
echo " XXX All processes completed."
#
#==========================================================================
