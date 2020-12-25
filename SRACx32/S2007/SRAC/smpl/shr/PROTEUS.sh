#!/bin/csh
#
#===============================================================================
#  <<  run SRAC  >>
#  PROTEUS.sh :
#    Reaction rate calculation in PROTEUS-LWHCR experiments at PSI
#    Core 6 : Two rod heterogeneous lattce with 15%PuO2/UO2 and 
#             Depleted UO2, moderated by H2O (0% void)
#  Options : Pij(Geometry type:IGT=13), Plot geometry, PEACO, Reaction Rate
#===============================================================================
#
   alias   mkdir mkdir
   alias   cat   cat
   alias   cd    cd
   alias   rm    rm
#
#============= Set by user =====================================================
#
#  LMN    : executable command of SRAC (SRAC/bin/*)
#  BRN    : burnup chain data          (SRAC/lib/burnlibT/*)
#  ODR    : directory in which output data will be stored
#  CASE   : case name which is refered as name of output files and PDS directory
#  WKDR   : working directory in which scratch files will be made and deleted
#  PDSD   : top directory name of PDS file
#
   set SRAC_DIR = $HOME/code/SRAC2K6/SRAC
   set LMN  = SRAC.100m
   set BRN  = u4cm6fp50bp16T
   set ODR  = $SRAC_DIR/smpl/outp
   set CASE = PROTEUS
   set PDSD = $SRAC_DIR/tmp
#
#=============  mkdir for PDS  =================================================
#
#  PDS_DIR : directory name of PDS files
#  PDS file names must be identical with those in input data
#
   set PDS_DIR = $PDSD/$CASE
   mkdir $PDS_DIR
   mkdir $PDS_DIR/UFAST
   mkdir $PDS_DIR/UTHERMAL
   mkdir $PDS_DIR/UMCROSS
   mkdir $PDS_DIR/MACROWRK
   mkdir $PDS_DIR/MACRO
   mkdir $PDS_DIR/FLUX
   mkdir $PDS_DIR/MICREF
#  
#=============  Change if you like =============================================
#
   set LM       = $SRAC_DIR/bin/$LMN
   set DATE     = `date +%Y.%m.%d.%H.%M.%S`
   set WKDR     = $HOME/SRACtmp.$CASE.$DATE
   mkdir $WKDR
#
#-- File allocation
#  fu89 is used in any plot options, fu98 is used in the burnup option
#  Add other units if you would like to keep necessary files.
   setenv  fu50  $SRAC_DIR/lib/burnlibT/$BRN
   setenv  fu85  $SRAC_DIR/lib/kintab.dat
   setenv  fu89  $ODR/$CASE.SFT89.$DATE
#  setenv  fu98  $ODR/$CASE.SFT98.$DATE
   setenv  fu99  $ODR/$CASE.SFT99.$DATE
   set OUTLST =  $ODR/$CASE.SFT06.$DATE
#
#=============  Exec SRAC code with the following input data ===================
#
cd $WKDR
cat - << END_DATA | $LM >& $OUTLST
COR6
PROTEUS-LWHCR Experiment (Core 6, 0% Void)
1 1 1 1 2   1 4 3 -12 1   0 0 0 0 1   2 1 1 0 0 / SRAC Control
2.0507E-3  / Buckling (Critical searced value)
*- PDS files ------2---------3---------4---------5---------6---------7--
* Note : All input line must be written in 72 columns except comments
*        even when environmental variables are expanded.
/home/okumura/code/SRAC2K6/SRACLIB-JDL33/pds/pfast   Old  File
/home/okumura/code/SRAC2K6/SRACLIB-JDL33/pds/pthml   O    F
/home/okumura/code/SRAC2K6/SRACLIB-JDL33/pds/pmcrs   O    F
$PDS_DIR/UFAST     Scratch  Core
$PDS_DIR/UTHERMAL  S        C
$PDS_DIR/UMCROSS   S        C
$PDS_DIR/MACROWRK  S        C
$PDS_DIR/MACRO     S        C
$PDS_DIR/FLUX      S        C
$PDS_DIR/MICREF    S        C
************************************************************************
62 45  1 1 /  107 group => 2 group
62(1)      /  Energy group structure suggested for LWR analyses
45(1)      /
62         / Fast    1 group
45         / Thermal 1 group
***** Enter one blank line after input for energy group structure

***** Input for PIJ (Collision Probability Method)
13 32 5 5 1   1 6 2 10 0   3 0 10 29 2   1 180 1    / Pij Control
0 20 50 5 5 5 -1  0.0001 0.00001 0.001 1.0 10. 0.5  /
  5  5  5  5  5  5                       &
  5  5  5  5  5  5                       &
   1 2  3 4  1 2                         &
 3 4  1 2  1 2  3 4                      &
   1 2  3 4  1 2                         & T-S
5(1)                       /  X-R
1 2 3 4 5                  /  M-R
0.0  6*0.45                /  RX
0.0  2*0.77942             /  TY
2 4 6   1 3 5 7  2 4 6     /  IXP
1 1 1   2 2 2 2  3 3 3     /  IYP 
10( 0.0  0.335  0.411 )    /  RDP
12 1 1                     /  Plot Pij Geometry
****** Input for material specification
5 / NMAT
FUE1X01X  0 18 300. 0.67  0.58755  /  MAT  1 : Fuel (15% PuO2/UO2)
XU050000  2 0 7.781E-05  / 1
XU080000  2 0 1.839E-02  / 2
XPU90000  2 0 2.580E-03  / 3
XPU00000  2 0 5.699E-04  / 4
XPU10000  2 0 5.675E-05  / 5
XPU20000  2 0 1.256E-05  / 6
XAM10000  2 0 3.833E-05  / 7
XU030000  2 0 1.0E-12    / 8 Dummy for Reaction Rate Calculation
XTH20000  2 0 1.0E-12    / 9 Dummy
XO060000  0 0 4.346E-02  /10
XH010000  0 0 2.005E-04  /11
XAL70000  0 0 3.683E-04  /12
XFEN0000  0 0 2.600E-03  /13
XCRN0000  0 0 6.843E-04  /14
XNIN0000  0 0 3.301E-04  /15
XMN50000  0 0 5.376E-05  /16
XSIN0000  0 0 3.286E-05  /17
XMON0000  0 0 8.123E-06  /18
FUE2X02X  0 8 300. 0.10   1.0      /  MAT 2 : Cladding (steel+air+Al)
XAL70000  0 0 6.080E-03  / 1
XFEN0000  0 0 3.125E-02  / 2
XCRN0000  0 0 8.536E-03  / 3
XNIN0000  0 0 5.118E-03  / 4
XMN50000  0 0 1.001E-03  / 5
XMON0000  0 0 7.354E-04  / 6
XSIN0000  0 0 8.124E-04  / 7
XN040000  0 0 1.323E-05  / 8
MAT3X03X  0 11 300. 0.67  0.58755  /  MAT 3 : Depleted UO2
XU050000  2 0 9.851E-05  / 1
XU080000  2 0 2.328E-02  / 2
XPU90000  2 0 1.0E-12    / 3 Dummy
XPU00000  2 0 1.0E-12    / 4 Dummy
XPU10000  2 0 1.0E-12    / 5 Dummy
XPU20000  2 0 1.0E-12    / 6 Dummy
XAM10000  2 0 1.0E-12    / 7 Dummy
XU030000  2 0 1.0E-12    / 8 Dummy
XTH20000  2 0 1.0E-12    / 9 Dummy
XO060000  0 0 4.677E-02  /10
XAL70000  0 0 3.827E-04  /11
MAT4X04X  0 8 300. 0.10   1.0      /  MAT 4 : Cladding (steel+air+Al)
XAL70000  0 0 6.080E-03  / 1
XFEN0000  0 0 3.125E-02  / 2
XCRN0000  0 0 8.536E-03  / 3
XNIN0000  0 0 5.118E-03  / 4
XMN50000  0 0 1.001E-03  / 5
XMON0000  0 0 7.354E-04  / 6
XSIN0000  0 0 8.124E-04  / 7
XN040000  0 0 1.323E-05  / 8
MAT5X05X  0 2 300. 0.10  1.0       /  MAT 5 : Light water (300 D K)
XH01H000  0 0 6.652E-02  / 1
XO060000  0 0 3.326E-02  / 2
****** Input for Reaction rate calculation
* thermal cut-off energy = 0.99312eV
0 0 10 0  / IOPT(1:4)--------- << Reaction Rate >> -------------------+
1 1 2 1 0 0 67(1.0) 40(0.0) / MPOSI,LU235,LU238,IX,IY,IZ,FGS(1:IGMAX) !
1 3 4 1 0 0 67(1.0) 40(0.0) /                                         !
1 5 6 1 0 0 67(1.0) 40(0.0) /                                         !
1 7 8 1 0 0 67(1.0) 40(0.0) /                                         !
1 9 9 1 0 0 67(1.0) 40(0.0) /                                         !
3 1 2 3 0 0 67(1.0) 40(0.0) /                                         !
3 3 4 3 0 0 67(1.0) 40(0.0) /                                         !
3 5 6 3 0 0 67(1.0) 40(0.0) /                                         !
3 7 8 3 0 0 67(1.0) 40(0.0) /                                         !
3 9 9 3 0 0 67(1.0) 40(0.0) / ----------------------------------------+
****** Input for PEACO option
0    / no plot

END_DATA
#
#========  Remove scratch files ==============================================
#
   cd $HOME
   rm -r $WKDR
#
#========  Remove PDS files if you don't keep them ===========================
#
   rm -r $PDS_DIR
#
#  rm -r $PDS_DIR/UFAST
#  rm -r $PDS_DIR/UTHERMAL
#  rm -r $PDS_DIR/UMCROSS
#  rm -r $PDS_DIR/MACROWRK
#  rm -r $PDS_DIR/MACRO
#  rm -r $PDS_DIR/FLUX
#  rm -r $PDS_DIR/MICREF
