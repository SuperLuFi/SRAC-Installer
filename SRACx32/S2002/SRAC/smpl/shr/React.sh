#!/bin/csh
#
##################################################################
#                                 
#  <<  run SRAC  >>
#                                   
#  by Keisuke OKUMURA (E-mail:okumura@mike.tokai.jaeri.go.jp)
#                                
##################################################################
#  sample problem React : Pij with Reaction Rate Cal. Option 
##################################################################
#    
# Fortran logical unit usage (allocate if you need)
#
#       The meaning of each file depends on sub-programs used in SRAC.
#       [ ]:important files for users. 
# 
#   1   binary (ANISN,TWOTRAN,CIATION)
#   2   binary (ANISN,CITATION), scratch
#   3   binary (SRAC,ANISN,TWOTRAN,CITATION), scratch
#   4   binary (PIJ,ANISN,TWOTRAN), scratch
# [ 5]  text:80 standard input
# [ 6]  text:137 standard output, monitoring message
#   8   binary (ANISN,TWOTRAN), angular flux in TWOTRAN
#   9   binary (TWOTRAN,CITATION)
#               flux map in CITATION, angular flux in TWOTRAN
#  10   binary (ANISN,TWOTRAN,CITATION), scratch
#  11   binary (TWOTRAN,CITATION), Sn constants in TWOTRAN
#  12   binary (TWOTRAN), restart file for TWOTRAN
#  13   binary (TWOTRAN,CITATION), restart file for TWOTRAN & CITATION
#  14   binary (TWOTRAN,CITATION), scratch
#  15   binary (CITATION), scratch (fast I/O device may be effective)
#  16   binary (CITATION), scratch
#  17   binary (CITATION), fixed source in CITATION
#  18   binary (CITATION), scratch
#  19   binary (CITATION), scratch 
#  20   binary (CITATION), scratch
#  21   binary (PIJ), scratch
#  22   binary (PIJ,CITATION), scratch
#  26   binary (CITATION), scratch
#  28   binary (CITATION), scratch
#  31   text:80 (SRAC-CVMACT,CITATION), macro-XS interface for CITATION
#  32   binary (PIJ,ANISN,TWOTRAN,TUD,CITATION)
#               fixed source for TWOTRAN, power density map in CITATION 
#  33   binary (PIJ,TWOTRAN,TUD), total flux in TWOTRAN & TUD
#  49   device internally used to access PDS file
# [50]  text:80 burnup chain library (SRAC-BURNUP) 
#  52   binary (SRAC-BURNUP), scratch
#  81   binary (PIJ), scratch
#  82   binary (PIJ), scratch
#  83   binary (PIJ), scratch
#  84   binary (PIJ), scratch
#  85   binary data table (PIJ), always required in PIJ
# [89]  plot file : PostScript (SRAC-PEACO,PIJ)
#  91   text:80 (CITATION), scratch
#  92   binary (CITATION), scratch
#  93   text:80 (SRAC-BURNUP), scratch
#  95   text:80 (SRAC-DTLIST), scratch
#  96   binary (SRAC-PEACO), scratch
#  97   binary (SRAC-BURNUP), scratch
# [98]  text:137 (SRAC-BURNUP) summary of burnup results
# [99]  text:137 calculated results
#
#=============================================================
#
   alias   mkdir mkdir
   alias   cat   cat
   alias   cd    cd
   alias   rm    rm
#
#============= Set by user ===================================
#
#  LMN    : load module name
#           = SRACsc.30m(Scalar,30M), SRACvp.50m(Vector,50M), ....
#  BRN    : burnup chain library data
#           =ucm66fp  : U-Np-Pu-Am-Cm & 65+1 FP & B-10 (standard model)
#           =thcm66fp : Th-Pa-U-Np-Pu-Cm & 65+1 FP & B-10 (Th model)
#           =ucm34fp  : U-Np-Pu-Am-Cm & 30+4 FP & B-10 (simple FP model)
#  ODR    : directory name in which output data will be stored 
#  CASE   : case name which is refered as names of output files and PDS
#  WKDR   : directory name in which scratch PS files will be made and deleted
#  PDSD   : directory name in which PDS files will be made
#     
   set LMN  = SRACsc.30m
   set BRN  = ucm66fp
   set ODR  = $HOME/SRAC/smpl/outp
   set CASE = React
   set PDSD = $HOME/SRAC/tmp
#
#=============  mkdir for PDS  ================================
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
#=============  Change if you like ============================
#
   set SRAC_DIR = $HOME/SRAC
   set LM       = $SRAC_DIR/bin/$LMN
   set DATE     = `date +%b%d.%H.%M.%S`
   set WKDR     = $HOME/SRACtmp.$CASE.$DATE
   mkdir $WKDR
#
   setenv  fu50  $SRAC_DIR/lib/burnlibT/$BRN
   setenv  fu85  $SRAC_DIR/lib/kintab.dat
   setenv  fu89  $ODR/$CASE.SFT89.$DATE
#  setenv  fu98  $ODR/$CASE.SFT98.$DATE
   setenv  fu99  $ODR/$CASE.SFT99.$DATE
   set OUTLST =  $ODR/$CASE.SFT06.$DATE
#
#=============  Exec SRAC code with the following input data =============
#
cd $WKDR
cat - << END_DATA | $LM >& $OUTLST
COR6
LWHCLR Moderator-Voidage Analysis : PROTEUS-CORE 6 (H2O)
1 1 1 1 2   1 4 3 -12 1   0 0 0 0 2   0 1 1 0 0 / SRAC CONTROL
3.5000E-3  / BUCKLING (Replaced by Critical Searced Value)
$HOME/SRACLIB-JDL32/pds/pfast   Old  File
$HOME/SRACLIB-JDL32/pds/pthml   O    F   
$HOME/SRACLIB-JDL32/pds/pmcrs   O    F   
$PDS_DIR/UFAST            Scratch  Core
$PDS_DIR/UTHERMAL         S        C
$PDS_DIR/UMCROSS          S        C
$PDS_DIR/MACROWRK         S        C
$PDS_DIR/MACRO            S        C
$PDS_DIR/FLUX             S        C
$PDS_DIR/MICREF           S        C
& Caution : Directory for PDS will not be made or deleted in program.
&           If you set Scratch, members will be deleted. 
66 20  3 1    /  89-gruop 
62(1) 4(2)    /
4(1) 15(2) 3  /
28 21 17      /
20            /

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
12 1 1                     /  PLOT PIJ GEOMETRY
5 / NMAT
MAT1X01X  0 18 300. 0.67  0.58755  /  MAT  1 : FUEL ROD(15% PU2/UO2)
XU050001  2 0 7.781E-05  / 1
XU080001  2 0 1.839E-02  / 2
XPU90001  2 0 2.580E-03  / 3
XPU00001  2 0 5.699E-04  / 4
XPU10001  2 0 5.675E-05  / 5
XPU20001  2 0 1.256E-05  / 6
XAM10001  2 0 3.833E-05  / 7
XU030001  2 0 1.0E-12    / 8 DUMMY for Reaction Rate Calculation
XTH20001  2 0 1.0E-12    / 9 DUMMY
XO060001  0 0 4.346E-02  /10
XH010001  0 0 2.005E-04  /11
XAL70001  0 0 3.683E-04  /12
XFEN0001  0 0 2.600E-03  /13
XCRN0001  0 0 6.843E-04  /14
XNIN0001  0 0 3.301E-04  /15
XMN50001  0 0 5.376E-05  /16
XSIN0001  0 0 3.286E-05  /17
XMON0001  0 0 8.123E-06  /18
MAT2X02X  0 8 300. 0.10   1.0      /  MAT  2 : CLADDING (STEEL+AIR+AL)
XAL70001  0 0 6.080E-03  / 1
XFEN0001  0 0 3.125E-02  / 2
XCRN0001  0 0 8.536E-03  / 3
XNIN0001  0 0 5.118E-03  / 4
XMN50001  0 0 1.001E-03  / 5
XMON0001  0 0 7.354E-04  / 6
XSIN0001  0 0 8.124E-04  / 7
XN040001  0 0 1.323E-05  / 8
MAT3X03X  0 11 300. 0.67  0.58755  /  MAT  3 : DEPLETED UO2
XU050001  2 0 9.851E-05  / 1
XU080001  2 0 2.328E-02  / 2
XPU90001  2 0 1.0E-12    / 3 DUMMY
XPU00001  2 0 1.0E-12    / 4 DUMMY
XPU10001  2 0 1.0E-12    / 5 DUMMY
XPU20001  2 0 1.0E-12    / 6 DUMMY
XAM10001  2 0 1.0E-12    / 7 DUMMY
XU030001  2 0 1.0E-12    / 8 DUMMY
XTH20001  2 0 1.0E-12    / 9 DUMMY
XO060001  0 0 4.677E-02  /10
XAL70001  0 0 3.827E-04  /11
MAT4X04X  0 8 300. 0.10   1.0      /  MAT  4 : CLADDING (STEEL+AIR+AL)
XAL70001  0 0 6.080E-03  / 1
XFEN0001  0 0 3.125E-02  / 2
XCRN0001  0 0 8.536E-03  / 3
XNIN0001  0 0 5.118E-03  / 4
XMN50001  0 0 1.001E-03  / 5
XMON0001  0 0 7.354E-04  / 6
XSIN0001  0 0 8.124E-04  / 7
XN040001  0 0 1.323E-05  / 8
MAT5X05X  0 2 300. 0.10  1.0      /  MAT  5 : LIGHT WATER (300 D K)
XH01H001  0 0 6.652E-02  / 1
XO060001  0 0 3.326E-02  / 2
0 0 10 0  / IOPT(1:4)--------- << REACTION RATE >> -------------------+
1 1 2 1 0 0 67(1.0) 19(0.0) / MPOSI,LU235,LU238,IX,IY,IZ,FGS(1:IGMAX) !
1 3 4 1 0 0 67(1.0) 19(0.0) /                                         !
1 5 6 1 0 0 67(1.0) 19(0.0) /                                         !
1 7 8 1 0 0 67(1.0) 19(0.0) /                                         !
1 9 9 1 0 0 67(1.0) 19(0.0) /                                         !
3 1 2 3 0 0 67(1.0) 19(0.0) /                                         !
3 3 4 3 0 0 67(1.0) 19(0.0) /                                         !
3 5 6 3 0 0 67(1.0) 19(0.0) /                                         !
3 7 8 3 0 0 67(1.0) 19(0.0) /                                         !
3 9 9 3 0 0 67(1.0) 19(0.0) / ----------------------------------------+
0  / PEACO PLOT


END_DATA
#
#========  Remove scratch PS files ===========================================
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
