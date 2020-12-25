#!/bin/csh
#
##################################################################
#                                 
#  <<  run SRAC  >>
#                                   
#  by Keisuke OKUMURA (E-mail:okumura@mike.tokai.jaeri.go.jp)
#                                
##################################################################
#  sample problem CitTri : Pij => Citation 2D (Triangular Mesh)  
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
   set CASE = CitTri
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
HFA0
MACRO FOR HCLWR CORE CALCULATION (6.5WT.%)
1 1 1 1 2   1 4 3 -2 1   0 0 0 0 2   0 3 0 0 0 / SRAC CONTROL
3.6273E-4   / BUCKLING
$HOME/SRACLIB-JDL32/pds/pfast   Old  File
$HOME/SRACLIB-JDL32/pds/pthml   O    F   
$HOME/SRACLIB-JDL32/pds/pmcrs   O    F   
$PDS_DIR/UFAST            Scratch  Core
$PDS_DIR/UTHERMAL         S        C
$PDS_DIR/UMCROSS          S        C
$PDS_DIR/MACROWRK         S        C
$PDS_DIR/MACRO            N        C
$PDS_DIR/FLUX             S        C
$PDS_DIR/MICREF           S        C
59 31  5 2    /
59(1)         /
15(1) 15(2) 3 /
10 10 8 17 14 /
15 16         /

6 6 6 3 1   1 6 0 0 0  -5 0 6 15 0   0 30 0           / Pij Control
0 100 100 5 5 5 -1  0.0001 0.00001 0.001 1.0 10. 0.5  /
1 1 1 2 3 3  / R-T
3(1)         / R-X
1 2 3        / M-R
0.0  0.2511  0.3552  0.435  0.475  0.530  0.5946     / RX
3 / NMAT
FUE1P0PP  0 8  1000.  0.870    0.0  / 1 : FUEL FIRST REGION
XPU90009  2 0  1.1854E-3       /1         (6.5WT.%PU-F)
XPU00009  2 0  5.0889E-4       /2
XPU10009  2 0  2.2481E-4       /3
XPU20009  2 0  1.1445E-4       /4
XAM10009  2 0  1.0219E-5       /5
XU050009  2 0  4.0019E-5       /6
XU08W009  2 0  1.9718E-2       /7
XO06W009  0 0  4.3603E-2       /8
CLD1P0PP  0 4  600.   0.08     0.0  / 2 : CLADDING
XFEN0008  2 0  5.9620E-2       /1        (SUS-304)
XCRN0008  2 0  1.7380E-2       /2
XNIN0008  2 0  8.0980E-3       /3
XMN50008  2 0  8.6590E-4       /4
MOD1Q0QQ  0 6  581.   1.0      0.0  / 3 : MODERATOR
XH01H008  0 0  4.21634E-2      /1    
XO060008  0 0  2.10817E-2      /2   
XFEN0008  0 0  6.70725E-3      /5
XCRN0008  0 0  1.95525E-3      /4
XNIN0008  0 0  9.11025E-4      /3
XMN50008  0 0  9.74138E-5      /6
0  / PEACO
HFB0
MACRO (HIGH TEMP.,FULL POWER ,SECOND REGION(B),0% VOID)
1 1 1 1 2   1 4 3 -2 1   1 0 0 0 2   0 3 0 0 0 / SRAC CONTROL  
3.6273E-4  / GEOMETRICAL BUCKLING
3 / NMAT
FUE2R0RR  0 8  1000.  0.870    0.0  / 1 : FUEL SECOND REGION
XPU90009  2 0  1.3637E-3       /1         (7.5WT.%PU-F)
XPU00009  2 0  6.4402E-4       /2
XPU10009  2 0  2.6346E-4       /3
XPU20009  2 0  1.5613E-4       /4
XAM10009  2 0  1.2197E-5       /5
XU050009  2 0  3.9212E-5       /6
XU08W009  2 0  1.9320E-2       /7
XO06W009  0 0  4.3598E-2       /8
CLD2R0RR  0 4  600.   0.08     0.0  / 2 : CLADDING
XFEN0008  2 0  5.9620E-2       /1        (SUS-304)
XCRN0008  2 0  1.7380E-2       /2
XNIN0008  2 0  8.0980E-3       /3
XMN50008  2 0  8.6590E-4       /4
MOD2S0SS  0 6  581.   1.0      0.0  / 3 : MODERATOR
XH01H008  0 0  4.21634E-2      /1 
XO060008  0 0  2.10817E-2      /2 
XFEN0008  0 0  6.70725E-3      /3
XCRN0008  0 0  1.95525E-3      /4
XNIN0008  0 0  9.11025E-4      /5
XMN50008  0 0  9.74138E-5      /6
0  / PEACO
HFC0
MACRO (HIGH TEMP.,FULL POWER ,THIRD REGION(C),0% VOID)
1 1 1 1 2   1 4 3 -2 1   1 0 0 0 2   0 3 0 0 0 / SRAC CONTROL  
3.6273E-4  / GEOMETRICAL BUCKLING
4 / NMAT
FUE3U0UU  0 8  1000.  0.870    0.0  / 1 : FUEL THIRD REGION
XPU90009  2 0  1.6413E-3       /1         (9.0WT.%PU-F)
XPU00009  2 0  7.0464E-4       /2
XPU10009  2 0  3.1129E-4       /3
XPU20009  2 0  1.5847E-4       /4
XAM10009  2 0  1.4149E-5       /5
XU050009  2 0  3.8417E-5       /6
XU08W009  2 0  1.8929E-2       /7
XO06W009  0 0  4.3594E-2       /8
CLD3U0UU  0 4  600.   0.08     0.0  / 2 : CLADDING
XFEN0008  2 0  5.9620E-2       /1        (SUS-304)
XCRN0008  2 0  1.7380E-2       /2
XNIN0008  2 0  8.0980E-3       /3
XMN50008  2 0  8.6590E-4       /4
MOD3V0VV  0 6  581.   1.0      0.0  / 3 : MODERATOR
XH01H008  0 0  4.21634E-2      /1   
XO060008  0 0  2.10817E-2      /2  
XFEN0008  0 0  6.70725E-3      /3
XCRN0008  0 0  1.95525E-3      /4
XNIN0008  0 0  9.11025E-4      /5
XMN50008  0 0  9.74138E-5      /6
REF0X0XX  0 6  581.   1.0      0.0  / 4 : REFLECTOR
XH01H008  0 0  3.32556E-2      /1 
XO060008  0 0  1.66278E-2      /2 
XFEN0008  0 0  1.78860E-2      /3
XCRN0008  0 0  5.21400E-3      /4
XNIN0008  0 0  2.42940E-3      /5
XMN50008  0 0  2.59770E-4      /6
0  / PEACO
CITA
TRIANGULAR 1/6-CORE:34*17 (2D BY CITATION)
0 0 0 1 0   0 0 0 0 1   0 5 0 0 2   0 1 0 0 0 / SRAC CONTROL
1.9847E-4  / Buckling (Not effective)
57 0 -3    / CITATION : 57-Zones to edit assembly power
1  1       / Material Dependent Spectrum & Kinetics Parameter Option
56(5) 1    / Benoist-D for Fuel Region
TRIANGULAR GEOMETRY (34*17) , 7-GROUPS , B.C. : 60DEG.ROTATIONAL
HIGH TEM. HIGH POWER 0% VOID  ( FIRST CYCLE NO BURN-UP)
001
  0  0  0  0  0  0  0  0  0  0  0  1  0  0  1  0  0  0  0  0  0  0
  1  0  1  1  0  0  0  0  1  0  0  1  1  1  0  0  0  0  0
  0
  0.
003
  0  0  0  0 10  0  0  0  0  0  0  0  2  2  0  0 58
  0.00001   0.00001                 0.00001
  0.        0.0         3411.0      1.0         0.000747384
004
 34 202.1881
 17 202.1881
005
 58
006
   1
  34  34  17  17   0   0
   2
  32  34  16  16   0   0  31  33  17  17   0   0
   3
  28  30  17  17   0   0
   4
  30  32  15  15   0   0  29  31  16  16   0   0
   5
  34  34  14  14   0   0  33  34  15  15   0   0
   6
  26  28  16  16   0   0  25  27  17  17   0   0
   7
  28  30  14  14   0   0  27  29  15  15   0   0
   8
  32  34  13  13   0   0  31  33  14  14   0   0
   9
  22  24  17  17   0   0
  10
  24  26  15  15   0   0  23  25  16  16   0   0
  11
  26  28  13  13   0   0  25  27  14  14   0   0
  12
  30  32  12  12   0   0  29  31  13  13   0   0
  13
  34  34  11  11   0   0  33  34  12  12   0   0
  14
  20  22  16  16   0   0  19  21  17  17   0   0
  15
  22  24  14  14   0   0  21  23  15  15   0   0
  16
  24  26  12  12   0   0  23  25  13  13   0   0
  17
  28  30  11  11   0   0  27  29  12  12   0   0
  18
  32  34  10  10   0   0  31  33  11  11   0   0
  19
  16  18  17  17   0   0
  20
  34  34   8   8   0   0  33  34   9   9   0   0
  21       / SECOND REGION FUELB001
  18  20  15  15   0   0  17  19  16  16   0   0
  22
  20  22  13  13   0   0  19  21  14  14   0   0
  23
  22  24  11  11   0   0  21  23  12  12   0   0
  24
  26  28  10  10   0   0  25  27  11  11   0   0
  25
  30  32   9   9   0   0  29  31  10  10   0   0
  26
  14  16  16  16   0   0  13  15  17  17   0   0
  27
  16  18  14  14   0   0  15  17  15  15   0   0
  28
  18  20  12  12   0   0  17  19  13  13   0   0
  29
  20  22  10  10   0   0  19  21  11  11   0   0
  30
  24  26   9   9   0   0  23  25  10  10   0   0
  31
  28  30   8   8   0   0  27  29   9   9   0   0
  32
  32  34   7   7   0   0  31  33   8   8   0   0
  33
  10  12  17  17   0   0
  34
  12  14  15  15   0   0  11  13  16  16   0   0
  35
  14  16  13  13   0   0  13  15  14  14   0   0
  36
  26  28   7   7   0   0  25  27   8   8   0   0
  37
  30  32   6   6   0   0  29  31   7   7   0   0
  38
  34  34   5   5   0   0  33  34   6   6   0   0
  39       /  THIRD REGION FUELC001
  16  18  11  11   0   0  15  17  12  12   0   0
  40
  18  20   9   9   0   0  17  19  10  10   0   0
  41
  22  24   8   8   0   0  21  23   9   9   0   0
  42
   8  10  16  16   0   0   7   9  17  17   0   0
  43
  10  12  14  14   0   0   9  11  15  15   0   0
  44
  12  14  12  12   0   0  11  13  13  13   0   0
  45
  14  16  10  10   0   0  13  15  11  11   0   0
  46
  16  18   8   8   0   0  15  17   9   9   0   0
  47
  20  22   7   7   0   0  19  21   8   8   0   0
  48
  24  26   6   6   0   0  23  25   7   7   0   0
  49
  28  30   5   5   0   0  27  29   6   6   0   0
  50
  32  34   4   4   0   0  31  33   5   5   0   0
  51
   4   6  17  17   0   0
  52
   6   8  15  15   0   0   5   7  16  16   0   0
  53
   8  10  13  13   0   0   7   9  14  14   0   0
  54
  26  28   4   4   0   0  25  27   5   5   0   0
  55
  30  32   3   3   0   0  29  31   4   4   0   0
  56
  34  34   2   2   0   0  33  34   3   3   0   0
  57       / RADIAL REFLECTOR
  32  34   1   1   0   0  28  33   2   2   0   0  24  29   3   3   0   0
  23  25   4   4   0   0  22  24   5   5   0   0  18  23   6   6   0   0
  14  19   7   7   0   0  13  15   8   8   0   0  12  14   9   9   0   0
  11  13  10  10   0   0  10  12  11  11   0   0   6  11  12  12   0   0
   5   7  13  13   0   0   4   6  14  14   0   0   3   5  15  15   0   0
   2   4  16  16   0   0   1   3  17  17   0   0
0
008
 -7  7  7
024
  11.9847E-4
999

1 1 1 1 1   1 1 1 1 1   1 1 1 1 1   1 1 1 1 1
2 2 2 2 2   2 2 2 2 2   2 2 2 2 2   2 2 2
3 3 3 3 3   3 3 3 3 3   3 3 3 3 3   3 3 3   4 / MATERIAL NO.-ZONE
4   /NMAT
HFA0A010  0 0 0.0 0.0 0.0    / 6.5W/O
HFB0A010  0 0 0.0 0.0 0.0    / 7.5W/O
HFC0A010  0 0 0.0 0.0 0.0    / 9.0W/O
REF0A0X0  0 0 0.0 0.0 0.0    / REFLECTOR(HIGH TEMP)

 
END_DATA
#
#========  Remove scratch PS files ===========================================
#
   cd $HOME
   rm -r $WKDR
#
#========  Remove PDS files if you don't keep them ===========================
#
#  rm -r $PDS_DIR
#
   rm -r $PDS_DIR/UFAST
   rm -r $PDS_DIR/UTHERMAL
   rm -r $PDS_DIR/UMCROSS
   rm -r $PDS_DIR/MACROWRK
#  rm -r $PDS_DIR/MACRO
   rm -r $PDS_DIR/FLUX
   rm -r $PDS_DIR/MICREF
