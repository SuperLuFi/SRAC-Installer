#!/bin/csh
#
##################################################################
#                                 
#  <<  run SRAC  >>
#                                   
#  by Keisuke OKUMURA (E-mail:okumura@mike.tokai.jaeri.go.jp)
#                                
##################################################################
#  sample problem SnSmpl : Pij => ANISN => TWOTRAN
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
   set CASE = SnSmpl
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
#  setenv  fu89  $ODR/$CASE.SFT89.$DATE
#  setenv  fu98  $ODR/$CASE.SFT98.$DATE
   setenv  fu99  $ODR/$CASE.SFT99.$DATE
   set OUTLST =  $ODR/$CASE.SFT06.$DATE
#
#=============  Exec SRAC code with the following input data =============
#
cd $WKDR
cat - << END_DATA | $LM >& $OUTLST
CELL
CRAC:  60.0 G/L : TEMP 40 C : HNO3 1 NOL : 53.52 CM HIGHT : DOPPLER 1
1 1 1 1 2   1 4 3 -2 0   0 0 1 0 2   1 1 0 0 0 / SRAC CONTROL
2.37826E-02 / R"=  16.700 CM  H"=  56.949 CM
$HOME/SRACLIB-JDL32/pds/pfast   Old  File
$HOME/SRACLIB-JDL32/pds/pthml   O    F   
$HOME/SRACLIB-JDL32/pds/pmcrs   O    F   
$PDS_DIR/UFAST       Scratch  Core
$PDS_DIR/UTHERMAL    S        C
$PDS_DIR/UMCROSS     S        C
$PDS_DIR/MACROWRK    S        C
$PDS_DIR/MACRO       S        C
$PDS_DIR/FLUX        S        C
$PDS_DIR/MICREF      S        C
61 46 11  7 /
61(1)       /
46(1)       /
2 2 6 2 3 5 8 8 9 8 8 /
7 6 6 6 6 6 9         / 

3 20 20 20 2    2 20  0 0 0   1 0 10 30 0   0 60 0    / Pij Control
0 100 100 5 5 5 -1  0.0001 0.00001 0.001 1.0 10. 0.5  /
19(1) 2    / X-R
19(1) 2    / M-R
0.0  0.950 1.950 17*0.7500 15.0000 / RX
2  / NMAT
FUELX0FX  0  5 3.13150E+02 2.94000E+01 0.0 / FUEL  U= 60.0 G/CC
XH01H001  0  0  6.45244E-02
XN040001  0  0  9.02153E-04
XO060001  0  0  3.49747E-02
XU050001  2  0  1.41980E-04
XU080001  2  0  1.03898E-05
CLDDX0CX  0  7 2.93150E+02 6.00000E-01 0.0 / SUS304
XC020001  0  0  3.17290E-04
XSIN0001  0  0  1.69620E-03
XCRN0001  2  0  1.74080E-02
XMN50001  2  0  1.73430E-03
XFEN0001  2  0  5.78720E-02
XNIN0001  2  0  8.11160E-03
XS0N0001  0  0  4.45720E-05
0   / PEACO   
ANIS
WHOLE CORE 107G CALCULATION USING ANISN(S8P1) TO GET CONDENSED X-SEC.
0 0 0 1 0  0 0 0 0 0  0 -2 1 0 2  3 -1 0 0 0 / SRAC CONTROL
1.000E-15 / BUCKLING:NOT EFFECTIVE
15&
1 0 1 8 2  1 0 3 20 1  107 0 0 0 0  0 0 0 0 0  0 0 0 50 0  0 0 0 150 0
0 0 0 0 1  0
16*
0.00000E+00 0.00000E+00 1.00000E-05 1.42089E+00 5.39490E+01
0.00000E+00 0.00000E+00 0.00000E+00 0.00000E+00 5.00000E-01
5.00000E-05 5.00000E-02 1.00000E-02 0.00000E+00
00T
04*
0.0 0.950 1.950 17*0.750 15.000  / MESH BOUNDARY
08&
17(1) 2 2 3    / ZONE BY MESH
09&
    1  1  2    / MATERIAL BY ZONE
19&
    1  1  1    / P1 FOR ALL ZONES
27&
    1  2  3    / X-REGION BY ZONE
00T
2 / NMAT
CELLA012  0  0  0.0  0.0  0.0 / FUEL  U=60.0 G/CC
CELLA022  0  0  0.0  0.0  0.0 / SUS304
TWOC
WHOLE CORE 18 G CALCULATION USING TWOTRAN-2 (S8P1)
0 0 0 1 0   0 0 0 0 1   0 3 0 0 2   0 1 0 0 0 / SRAC CONTROL
1.000E-15 / ZERO BUCKLING
1 / NO OF TITLE CARD
WHOLE CORE 18 G CALCULATION USING TWOTRAN-2 (S8P1) : 2D-RZ : FORWARD
0 1 8 18 4   4 1 0 0 0   1 0 3 3 0  0 0 0 0 0    0 0 0 0 0  30 0 0
700 2   0 0 1 2 2  1 3 0 1 0  0 0    / 42I
0.00000E+00 0.00000E+00 0.00000E+00 0.00000E+00 0.00000E+00
0.00000E+00 1.00000E-05 1.00000E+00 0.00000E+00 0.00000E+00
 8  5  2  1    / FINE R-MESH
 2 25  2  1    / FINE Z-MESH
0.0   8.00   13.200  14.700  15.000  / R-MESH
0.0   1.50   52.449  53.949  54.249  / Z-MESH
  -2 -2 -2 -3
  -1 -1 -2 -3
  -2 -2 -2 -3
  -3 -3 -3 -3
  16(0)
3 / NMAT
ANISA010  0  0  0.0  0.0  0.0 / INNER FUEL
ANISA020  0  0  0.0  0.0  0.0 / OUTER FUEL
ANISA030  0  0  0.0  0.0  0.0 / SUS304
 
 
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
