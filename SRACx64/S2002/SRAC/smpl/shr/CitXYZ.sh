#!/bin/csh
#
##################################################################
#                                 
#  <<  run SRAC  >>
#                                   
#  by Keisuke OKUMURA (E-mail:okumura@mike.tokai.jaeri.go.jp)
#                                
##################################################################
#  sample problem CitXYZ : 3-Dimensional CITATION (X-Y-Z)
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
   set CASE = CitXYZ
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
FUL1
MACRO FOR INNER FUEL (3.2W/O UO2) BY PIJ
1 1 1 1 2   1 4 3 -2 1   0 0 0 0 2   0 1 0 0 0 / SRAC CONTROL
1.0000E-20 / BUCKLING
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
61 46   1 1   /
61(1)         /
46(1)         /
61            /
46            /

4 7 7 3 1   1 7 0 0 0   5 0 6 45 0   0 90 0          / PIJ CONTROL
0 100 50 5 5 5 -1  0.0001 0.00001 0.001 1.0 10. 0.5  /
1 1 1 2 3 3 3  /  R-T
3(1)           /  X-R
1 2 3          /  M-R
0.0 0.236714 0.334764 0.41  0.475  0.5267 0.5783 0.630   / RX
4  / NMAT
FUE1X0AX  0 3  900.   0.82     0.0  / 1 : INNER FUEL 3.2W/O
XU050009  2 0  7.2270E-4       /1
XU080009  2 0  2.1585E-2       /2
XO060009  0 0  4.4616E-2       /3
CLD1X0BX  0 3  600.   0.13     0.0  / 2 : CLADDING
XZRN0008  0 0  3.8032E-2       /1
XCRN0008  0 0  6.7152E-5       /2
XFEN0008  0 0  1.3129E-4       /3
MOD1X0CX  0 2  581.   1.0      0.0  / 3 : MODERATOR
XH01H008  0 0  4.7508E-2       /1
XO060008  0 0  2.3754E-2       /2
REFLX0DX  0 6  581.   1.0      0.0  / 4 : REFLECTOR
XH01H008  0 0  4.7508E-2       /1
XO060008  0 0  2.3754E-2       /2
XFEN0008  0 0  1.7886E-2       /3
XCRN0008  0 0  5.2140E-3       /4
XNIN0008  0 0  2.4294E-3       /5
XMN50008  0 0  2.5977E-4       /6
0  / PEACO
FUL2
MACRO FOR OUTER FUEL (2.1W/O UO2), SAME GEOMETRY WITH THE ABOVE CASE
1 1 1 1 2   1 4 3 -2 1   1 0 0 0 2   0 1 0 0 0 / SRAC CONTROL
1.0000E-20 / BUCKLING
3  / NMAT
FUE2X0EX  0 3  900.   0.82     0.0  / 1 : OUTER FUEL 2.1W/O
XU050009  2 0  4.7428E-4       /1
XU080009  2 0  2.1831E-2       /2
XO060009  0 0  4.4610E-2       /3
CLD2X0FX  0 3  600.   0.13     0.0  / 2 : CLADDING
XZRN0008  0 0  3.8032E-2       /1
XCRN0008  0 0  6.7152E-5       /2
XFEN0008  0 0  1.3129E-4       /3
MOD2X0GX  0 2  581.   1.0      0.0  / 3 : MODERATOR
XH01H008  0 0  4.7508E-2       /1
XO060008  0 0  2.3754E-2       /2
0  / PEACO
CORE
SAMPLE FOR CITATION-3D(X-Y-Z), 1/8 CORE
0 0 0 1 0  0 0 0 0 1  0 5 0 0 2  0 1 0 0 0 / SRAC CONTROL
1.0000E-20 / BUCKLING (NOT EFFECTIVE)
3 0 -1 / NM NXR ID
1 1    / IXKY IDELAY (CALCULATE KINETICS PARAMETERS)
5.0CM MESH SIZE IN EACH DIRECTION
EPS(FLUX) < 1.0E-4, EPS(KEFF) < 1.0E-5,  ZONE 4:BLACKNESS
001
  0  0  0  0  0  0  0  0  0  0  0  1  0  0  0  0  0  0  0  0  0  0
  1  0  0  0  0  0  0  0  0  0  0  1  1  0  0  0  0  0  0  0  0  0  0  1
900
  0.
003
  0  0  0  0 11  0  0  0  0  0  1  0  0  1  0  1  4  1  0  0  0  0  0  0
  0.0001    0.00001
  0.0       0.0         3411.0      1.0         0.1250
004
  2 10.00000  4 20.00000  4 20.00000  4 20.00000  4 20.00000  4 20.00000
  4 20.00000  4 20.00000  4 20.00000  0
  4 20.00000  4 20.00000  4 20.00000  4 20.00000  4 20.00000  4 20.00000
  4 20.00000  4 20.00000  2 10.00000  0
  4 20.00000 34 170.0000  0
005
  3  3  3  3  4  4  4  4  4
  3  3  3  3  3  3  4  4  4
  3  3  3  3  3  3  3  4  4
  3  3  3  3  3  3  3  3  4
  3  3  3  3  3  3  3  3  4
  3  3  3  3  3  3  3  3  3
  3  3  3  3  3  3  3  3  3
  3  3  3  3  3  3  3  3  3
  3  3  3  3  3  3  3  3  3    / AXIAL REFLECTOR
  3  3  3  3  4  4  4  4  4
  2  2  2  3  3  3  4  4  4
  1  1  2  2  2  3  3  4  4
  1  1  1  1  2  2  3  3  4
  1  1  1  1  1  2  2  3  4
  1  1  1  1  1  1  2  3  3
  1  1  1  1  1  1  2  2  3
  1  1  1  1  1  1  1  2  3
  1  1  1  1  1  1  1  2  3    / CORE
008
 -2  1  1
999

1 2 3   / MATTERIAL NO. BY ZONE
3       / NMAT FOR CORE
FUL1A010  0 0    0.0  0.0  0.0   / HOMOGENIZED INNER FUEL
FUL2A010  0 0    0.0  0.0  0.0   / HOMOGENIZED OUTER FUEL
REFLA0D0  0 0    0.0  0.0  0.0   / REFLECTOR


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
