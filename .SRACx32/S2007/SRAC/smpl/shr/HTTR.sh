#!/bin/csh
#
#===============================================================================
#  <<  run SRAC  >>
#  HTTR.sh     : Cell calculation for HTTR (High-Temperature Engineering Test 
#                Reactor in JAERI) with many coated fuel particles in fuel block
#                fuel (87C08) irradiated up to 31.4 GWd/t in a PWR (Mihama-3)
#  Options     : Pij(Geometry type IGT=12), PEACO, Double Heterogeity
#                Plot geometry
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
   set CASE = HTTR
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
HTTR 
HTTR fuel element cell with burnable poison
1 1 1 1 2   1 4 3 -2 1   0 0 0 0 1   2 1 0 0 0 / SRAC CONTROL
5.0795E-4  / Geometrical buckling for P1/B1 calculation
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
62 45  3 3 /  107 group => 6 group
62(1)      /
45(1)      /
16 21 25   /
 8 20 17   /
***** Enter one blank line after input for energy group structure

***** Input for PIJ (Collision Probability Method)
12 147 30 30 1   1 3 1 36 0   2 0 16 180 4   0 360 1 / Pij Control
0 50 50 5 5 5 -1  0.0001 0.00001 0.001 1.0 10. 0.5  /
 6( 1  2  3  4)
 6( 5  6  7  8)
 6( 9 10 11 12)
12(13 14 15 16)
   21 22 23 24    & Fuel for 33 pins  or  Dummy for 31 Pins   
   17 18 19 20
   21 22 23 24    & Fuel for 33 pins  or  Dummy for 31 Pins   
   25 25 25 25    & BP-Hole 1 (Dummy) 
   26 26 26 26    & BP-Hole 2 
   27 27 27 27    & BP-Hole 3
   28 29 30       /  T on each sub for Block-3
30(1)             /  X on each R for Block-5
4(6 -3 4 6) 
  6 -3 4 6  5 5 5 5   & Outest Pins for 31 Pin-Block
  6 7 7               & Dummy Hole, BP 
  6 5 8               /  M on each R for Block-6
0.0 1.26714 18.976726 18.65504  / RX
6(5.15)  6(8.699)  6(10.050)  12(13.23)  3(15.011)   & Fuel
3(15.50)                        / BP-Holes  RPP in block10
  0.0    60.0   120.0   180.0   240.0   300.0
 30.0    90.0   150.0   210.0   270.0   330.0
  0.0    60.0   120.0   180.0   240.0   300.0
 19.340  41.010  78.990 100.659 139.341 161.010
198.990 220.659 259.341 281.010 318.990 340.660 
60 180 300                      & Fuels 
0.0  120.0  240.0               / BP-Holes  Theta in Block-11
33(0.0 0.5 1.3 1.707156 2.05)   & Fuels
 3(0.0 0.4 0.5 0.6 0.7 )        / BP-Holes  RDP in Block-12
10 1 1                          / Block-13  Plot Option
1 3 1 0.03 0.0691987            / Block-14  Double Heterogeneity
8 / NMAT
KRNLXX1X  0 5  300.   0.04   0.845848  / 1 : Kernel
XU050001  2 0  1.4403E-3       /1  U-235
XU080001  2 0  2.2280E-2       /2  U-238
XO060001  0 0  4.7441E-2       /3  O 
XB000001  0 0  1.1114E-7       /4  B-10
XB010001  0 0  4.5129E-7       /5  B-11
CTMTXX2X  0 4  300.   0.0      0.0  / 2 : Coating+Matrix
XC02C001  0 0  8.2392E-2       /1  C
XSIN0001  0 0  2.4159E-3       /2  Si
XB000001  0 0  1.8360E-8       /3  B-10
XB010001  0 0  7.4550E-8       /4  B-11
CMPTXX3X  0 7  300.   2.215385  0.152057  / 3 : Compact   
XU050001  2 0  1.1736E-4       /1  U-235
XU080001  2 0  1.8155E-3       /2  U-238
XO060001  0 0  3.8657E-3       /3  O 
XC02C001  0 0  7.5678E-2       /4  C
XSIN0001  0 0  2.2191E-3       /5  Si
XB000001  0 0  2.5921E-8       /6  B-10
XB010001  0 0  1.0525E-7       /7  B-11
SLEVXX4X  0 3  300.   0.0      0.0  / 4: Sleeve deluted IG-110
XC02C001  0 0  8.6376E-2       /1  C
XB000001  0 0  7.5845E-9       /2  B-10
XB010001  0 0  3.0796E-8       /3  B-11
GRPHXX5X  0 3  300.   0.0      0.0  / 5: Graphite IG-110
XC02C001  0 0  8.8743E-2       /1  C
XB000001  0 0  7.7923E-9       /2  B-10
XB010001  0 0  3.1640E-8       /4  B-11
HELIXX6X  0 1  300    0.0      0.0  / 6: Helium
XHE40001  0 0  2.5039E-5       /1  Helium
BPAVXX7X  0 3  300    0.0      0.0  / 7: BP
XB000001  0 0  3.8210E-4       /1  B-10
XB010001  0 0  1.5515E-3       /2  B-11
XC02C001  0 0  7.7370E-2       /3  C
GRHEXX8X  0 4  300    0.0      0.0  / 8: Block+Helium
XC02C001  0 0  6.8203E-2       /1  C
XB000001  0 0  5.9887E-9       /2  B-10
XB010001  0 0  2.4317E-8       /3  B-11
XHE40001  0 0  2.5039E-6       /4  Helium
****** Input for PEACO option
0    / no plot
****** Enter one blank line to terminate repeatation on calculation cases

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
