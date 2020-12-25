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
#----------- Kintab --------------------------------------
    set TGT_DIR  = $SRAC_DIR/tool/kintab
    sed -f $cnvtr $TGT_DIR/Makefile.org  >  $TGT_DIR/Makefile
#----------- TXTtoPDS ------------------------------------
    set TGT_DIR  = $SRAC_DIR/util/pdscnvt/src/txttopds
    sed -f $cnvtr $TGT_DIR/MakeTP.org    >  $TGT_DIR/MakeTP
#----------- PDStoTXT ------------------------------------
    set TGT_DIR  = $SRAC_DIR/util/pdscnvt/src/pdstotxt
    sed -f $cnvtr $TGT_DIR/MakePT.org    >  $TGT_DIR/MakePT
#----------- PDSMDL(BnupEdit) ----------------------------
    set TGT_DIR  = $SRAC_DIR/util/pdsmdl/main/BnupEdit
    sed -f $cnvtr $TGT_DIR/Makefile.org  >  $TGT_DIR/Makefile
#----------- PDSMDL(FluxEdit) ----------------------------
    set TGT_DIR  = $SRAC_DIR/util/pdsmdl/main/FluxEdit
    sed -f $cnvtr $TGT_DIR/Makefile.org  >  $TGT_DIR/Makefile
#----------- PDSMDL(FluxPlot) ----------------------------
    set TGT_DIR  = $SRAC_DIR/util/pdsmdl/main/FluxPlot
    sed -f $cnvtr $TGT_DIR/Makefile.org  >  $TGT_DIR/Makefile
#----------- PDSMDL(MacroEdit) ---------------------------
    set TGT_DIR  = $SRAC_DIR/util/pdsmdl/main/MacroEdit
    sed -f $cnvtr $TGT_DIR/Makefile.org  >  $TGT_DIR/Makefile
#----------- PDSMDL(MicroEdit) ---------------------------
    set TGT_DIR  = $SRAC_DIR/util/pdsmdl/main/MicroEdit
    sed -f $cnvtr $TGT_DIR/Makefile.org  >  $TGT_DIR/Makefile
#----------- PDSMDL(AnisnXS)------------------------------
    set TGT_DIR  = $SRAC_DIR/util/pdsmdl/main/AnisnXS
    sed -f $cnvtr $TGT_DIR/Makefile.org  >  $TGT_DIR/Makefile
#---------------------------------------------------------
