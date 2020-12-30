#!/bin/csh
#
#===============================================================================
#  <<  run SRAC  >>
#  CitTri.sh   : Sample calculation for a 2-D Triangular mesh geometry with 
#                CITATION
#                PIJ => CITATION (2D Tri.)
#  Options     : Pij(Geometry type IGT=6), PEACO, CITATION (Tri.)
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
   set CASE = CitTri
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
#  setenv  fu89  $ODR/$CASE.SFT89.$DATE
#  setenv  fu98  $ODR/$CASE.SFT98.$DATE
   setenv  fu99  $ODR/$CASE.SFT99.$DATE
   set OUTLST =  $ODR/$CASE.SFT06.$DATE
#
#=============  Exec SRAC code with the following input data ===================
#
cd $WKDR
cat - << END_DATA | $LM >& $OUTLST
FUEA
Fuel cell for HCLWR core calculation (6.5 w/o Pu-Fiss)
1 1 1 1 2   1 4 3 -2 1   0 0 0 0 1   2 1 0 0 0 / SRAC CONTROL
3.6273E-4   / Geometrical buckling for P1/B1 calculation
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
62 45  8 8 /  107 group => 16 group
62(1)      /  Energy group structure suggested for LWR analyses
45(1)      /
8 11  9  9  9  9  4  3  / Fast 8 group
3  3  3  6  3 10  9  8  / Thermal 8 group
***** Enter one blank line after input for energy group structure

***** Input for PIJ (Collision Probability Method)
6 6 6 3 1   1 6 0 0 0  -5 0 6 15 0   0 30 0         / Pij Control
0 50 50 5 5 5 -1  0.0001 0.00001 0.001 1.0 10. 0.5  /
1 1 1 2 3 3  / R-T
3(1)         / R-X
1 2 3        / M-R
0.0  0.2511  0.3552  0.435  0.475  0.530  0.5946     / RX
****** Input for material specification
3 / NMAT
FUE1X0AX  0 8  1000.  0.870    0.0  / 1 : Fuel first region
XPU90000  2 0  1.1854E-3       /1         (6.5 w/o Pu-Fiss)
XPU00000  2 0  5.0889E-4       /2
XPU10000  2 0  2.2481E-4       /3
XPU20000  2 0  1.1445E-4       /4
XAM10000  2 0  1.0219E-5       /5
XU050000  2 0  4.0019E-5       /6
XU080000  2 0  1.9718E-2       /7
XO060000  0 0  4.3603E-2       /8
CLD1X0BX  0 4  600.   0.08     0.0  / 2 : Cladding
XFEN0000  2 0  5.9620E-2       /1         (SS304)
XCRN0000  2 0  1.7380E-2       /2
XNIN0000  2 0  8.0980E-3       /3
XMN50000  2 0  8.6590E-4       /4
MOD1X0CX  0 6  581.   1.0      0.0  / 3 : Moderator
XH01H000  0 0  4.21634E-2      /1
XO060000  0 0  2.10817E-2      /2   
XFEN0000  0 0  6.70725E-3      /5
XCRN0000  0 0  1.95525E-3      /4
XNIN0000  0 0  9.11025E-4      /3
XMN50000  0 0  9.74138E-5      /6
0  / PEACO
********************************************************************************
FUEB
Fuel cell for HCLWR core calculation (7.5 w/o Pu-fiss)
1 1 1 1 2   1 4 3 -2 1   1 0 0 0 1   2 1 0 0 0 / SRAC CONTROL
3.6273E-4  / Geometrical buckling for P1/B1 calculation
****** Input for material specification
3 / NMAT
FUE2X0DX  0 8  1000.  0.870    0.0  / 1 : Fuel second region
XPU90000  2 0  1.3637E-3       /1         (7.5 w/o Pu-Fiss)
XPU00000  2 0  6.4402E-4       /2
XPU10000  2 0  2.6346E-4       /3
XPU20000  2 0  1.5613E-4       /4
XAM10000  2 0  1.2197E-5       /5
XU050000  2 0  3.9212E-5       /6
XU080000  2 0  1.9320E-2       /7
XO060000  0 0  4.3598E-2       /8
CLD2X0EX  0 4  600.   0.08     0.0  / 2 : CLADDING
XFEN0000  2 0  5.9620E-2       /1        (SUS-304)
XCRN0000  2 0  1.7380E-2       /2
XNIN0000  2 0  8.0980E-3       /3
XMN50000  2 0  8.6590E-4       /4
MOD2X0FX  0 6  581.   1.0      0.0  / 3 : MODERATOR
XH01H000  0 0  4.21634E-2      /1 
XO060000  0 0  2.10817E-2      /2 
XFEN0000  0 0  6.70725E-3      /3
XCRN0000  0 0  1.95525E-3      /4
XNIN0000  0 0  9.11025E-4      /5
XMN50000  0 0  9.74138E-5      /6
0  / PEACO
********************************************************************************
FUEC
Fuel cell for HCLWR core calculation (9.0 w/o Pu-fiss)
1 1 1 1 2   1 4 3 -2 1   1 0 0 0 1   2 1 0 0 0 / SRAC CONTROL
3.6273E-4  / Geometrical buckling for P1/B1 calculation
****** Input for material specification
4 / NMAT
FUE3X0GX  0 8  1000.  0.870    0.0  / 1 : FUEL THIRD REGION
XPU90000  2 0  1.6413E-3       /1         (9.0 w/o Pu-Fiss)
XPU00000  2 0  7.0464E-4       /2
XPU10000  2 0  3.1129E-4       /3
XPU20000  2 0  1.5847E-4       /4
XAM10000  2 0  1.4149E-5       /5
XU050000  2 0  3.8417E-5       /6
XU080000  2 0  1.8929E-2       /7
XO060000  0 0  4.3594E-2       /8
CLD3X0HX  0 4  600.   0.08     0.0  / 2 : CLADDING
XFEN0000  2 0  5.9620E-2       /1        (SUS-304)
XCRN0000  2 0  1.7380E-2       /2
XNIN0000  2 0  8.0980E-3       /3
XMN50000  2 0  8.6590E-4       /4
MOD3X0IX  0 6  581.   1.0      0.0  / 3 : MODERATOR
XH01H000  0 0  4.21634E-2      /1   
XO060000  0 0  2.10817E-2      /2  
XFEN0000  0 0  6.70725E-3      /3
XCRN0000  0 0  1.95525E-3      /4
XNIN0000  0 0  9.11025E-4      /5
XMN50000  0 0  9.74138E-5      /6
REF0X0JX  0 6  581.   1.0      0.0  / 4 : REFLECTOR
XH01H000  0 0  3.32556E-2      /1 
XO060000  0 0  1.66278E-2      /2 
XFEN0000  0 0  1.78860E-2      /3
XCRN0000  0 0  5.21400E-3      /4
XNIN0000  0 0  2.42940E-3      /5
XMN50000  0 0  2.59770E-4      /6
0  / PEACO
********************************************************************************
CORE
Triangular 1/6-core: 34*17 (2D BY CITATION)
0 0 0 1 0   0 0 0 0 1   0 5 0 0 1   0 1 0 0 0 / SRAC CONTROL
1.9847E-4  / Buckling (not effective for core calculation)
****** Input for CITATION control
57 0 -1    / CITATION : 57-Zones to edit assembly power
1  1       / Material dependent spectrum & kinetics parameter option
****** Input for CITATION
Triangular geometry (34*17) , 16-Group , B.C. : 60-Deg. rotational
Hot Full Power 0% Void  ( First cycle no burn-up)
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
  21       / Second region FUELB001
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
  39       /  Third region FUELC001
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
  57       / Radial reflector
  32  34   1   1   0   0  28  33   2   2   0   0  24  29   3   3   0   0
  23  25   4   4   0   0  22  24   5   5   0   0  18  23   6   6   0   0
  14  19   7   7   0   0  13  15   8   8   0   0  12  14   9   9   0   0
  11  13  10  10   0   0  10  12  11  11   0   0   6  11  12  12   0   0
   5   7  13  13   0   0   4   6  14  14   0   0   3   5  15  15   0   0
   2   4  16  16   0   0   1   3  17  17   0   0
0
008
-16 16 16
024
  11.9847E-4
999

1 1 1 1 1   1 1 1 1 1   1 1 1 1 1   1 1 1 1 1
2 2 2 2 2   2 2 2 2 2   2 2 2 2 2   2 2 2
3 3 3 3 3   3 3 3 3 3   3 3 3 3 3   3 3 3   4 / Material No. by Zone
****** Input for material specification
4   /NMAT
FUEAA010  0 0 0.0 0.0 0.0    / 6.5 w/o fuel
FUEBA010  0 0 0.0 0.0 0.0    / 7.5 w/o fuel
FUECA010  0 0 0.0 0.0 0.0    / 9.0 w/o fuel
REF0A0J0  0 0 0.0 0.0 0.0    / Reflector

 
END_DATA
#
#========  Remove scratch files ================================================
#
   cd $HOME
   rm -r $WKDR
#
#========  Remove PDS files if you don't keep them =============================
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
