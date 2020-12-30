#!/bin/csh
#
#===============================================================================
#  <<  run SRAC  >>
#  Cell calculation for the Heavy Water Critical Assembly (DCA)
#  Sapmle input of Pij for CANDU type lattice geometry
#  Options : Pij(Geometry type:IGT=10), Plot geometry, PEACO
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
   set CASE = DCA
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
DCA1
DCA Cell 1.2w/o, 28 rod cluster, 22.5cm pitch
1 1 1 1 2   1 4 3 -2 1   0 0 0 0 1   2 1 0 0 0 / SRAC CONTROL
9.0E-4 / Axial buckling
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
*IGT  S  T  R  X   IBOUND NX NY NTPIN NAPIN NCELL EDITPIJ NGR NDA NDPIN
 10  27 27 10  1     0    12 0   28     3    1      0      6   21   2
*IDIVP IBETM IPLOT
   2    45     1   /   PIJ control integer
0 6(0) 6(0.)       /   Iteration parameters (defaulted values)
1 2  2 1  & First  4 pin rod ring                      R-T 1
1 3  3 1  & Second 8 pin rod ring                      R-T 2
1 4  4 1  & Third 16 pin rod ring                      R-T 3
5 5 5 6 6 6 6     & H2O coolant                        R-T 4
7 8 9             & Pressure tube, gap, calandria tube R-T 5
5(10)             / D2O moderator                      R-T 6
10(1)               / X-R
2 1 1 1 3 3 2 4 2 5 / M-R
4 8 16              / NPIN ON RING
0.0 2.16  3.88 5.60 5.8 6.0 6.625 6.825 7.3 8.4 9.5 11.0 12.694  /RX
1.312   2.999  4.757 / RPP : Radii of fuel ring
0.0    3*90.0        & Theta for first ring
22.5   7*45.0        & Theta for second ring
11.25 15*22.5        / Theta for third ring
0.0  0.720 0.835     / RDP : Pin rod division
6 2 1  / for geometry plot
****** Input for material specification
5  / NMAT
FUELX01X  0 3 300.  1.48  0. /  MAT 1 UO2 Fuel
XU050000  2 0 2.8060E-4   /
XU080000  2 0 2.2755E-2   /
XO060000  0 0 4.7387E-2   /
CLADX02X  0 2 300.  0.18  0. /  MAT 2 Cladding
XAL70000  0 0 5.8100E-2   /
XMGN0000  0 0 1.7200E-4   /
H2OMX0WX  0 2 300.  0.    0. /  MAT 3 H2O
XH01H000  0 0 6.7240E-2
XO060000  0 0 3.3360E-2
AGAPX03X  0 2 300.  0.    0. /  MAT 4 Air gap
XN040000  0 0 3.9600E-5
XO060000  0 0 1.0800E-5
D2OMX03X  0 3 300.  0.    0. /  MAT 5 D2O
XH01H000  0 0 3.6800E-5
XD02D000  0 0 6.6568E-2
XO060000  0 0 3.3220E-2
****** Input for PEACO option
0    / no plot

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
