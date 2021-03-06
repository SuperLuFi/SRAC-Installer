#!/bin/csh
#
#===============================================================================
#
#  <<  run SRAC  >>
#
#===============================================================================
#  Test.sh : Test problem to check SRAC installation
#            UO2 pin cell calculation in LWR next generation
#            fuel benchmark (No burn-up)
#  Options : Pij(Geometry type IGT=4), PEACO
#===============================================================================
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
   set SRAC_DIR = $HOME/SRAC
   set LMN  = SRAC.100m
   set BRN  = u4cm6fp50bp16T
   set ODR  = $SRAC_DIR/smpl/outp
   set CASE = Test
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
TEST : Case name (A4)
UO2 pin cell problem in LWR next generation fuel benchmark (No burn-up)
************************************************************************
*  Benchmark Reference :
*  A.Yamamoto, T.Ikehara, T.Ito, and E.Saji : "Benchmark Problem for
*  Reactor Physics Study of LWR Next Generation Fuels",
*  J. Nucl. Sci. Technol., Vol.39, No.8, pp.900-912, (2002).
************************************************************************
1 1 1 1 2   1 4 3 -2 1   0 0 0 0 1   2 1 0 0 0 / SRAC CONTROL
1.000E-20 / Geometrical buckling for P1/B1 calculation
*- PDS files ------2---------3---------4---------5---------6---------7--
* Note : All input line must be written in 72 columns except comments
*        even when environmental variables are expanded.
/$HOME/SRACLIB-JDL33/pds/pfast   Old  File
/$HOME/SRACLIB-JDL33/pds/pthml   O    F
/$HOME/SRACLIB-JDL33/pds/pmcrs   O    F
$PDS_DIR/UFAST      Scratch  Core
$PDS_DIR/UTHERMAL   S        C
$PDS_DIR/UMCROSS    S        C
$PDS_DIR/MACROWRK   S        C
$PDS_DIR/MACRO      S        C
$PDS_DIR/FLUX       S        C
$PDS_DIR/MICREF     S        C
************************************************************************
62 45  8 8 /  107 group => 16 group
62(1)      /  Energy group structure suggested for LWR analyses
45(1)      /
8 11  9  9  9  9  4  3  / Fast 8 group
3  3  3  6  3 10  9  8  / Thermal 8 group
***** Enter one blank line after input for energy group structure

***** Input for PIJ (Collision Probability Method)
4 7 7 3 1   1 7 0 0 0   5 0 6 15 0   0 45 0         / Pij Control
0 50 50 5 5 5 -1  0.0001 0.00001 0.001 1.0 10. 0.5  /
1 1 1 2 3 3 3  /  R-S
3(1)           /  X-R
1 2 3          /  M-R
0.0  0.238  0.336  0.412  0.476  0.528  0.580  0.6325    / RX
****** Input for material specification
3 / NMAT
FUELX01X  0 3  900.0  0.824    0.0  / 1 : UO2 fuel
XU050000  2 0  1.5122E-03      /1
XU080000  2 0  2.1477E-02      /2
XO060000  0 0  4.5945E-02      /3
CLADX02X  0 1  600.0  0.128    0.0  / 2 : cladding
XZRN0000  2 0  4.3107E-02      /1
MODEX03X  0 2  600.0  1.0      0.0  / 3 : moderator
XH01H000  0 0  4.4148E-02      /1
XO060000  0 0  2.2074E-02      /2
****** Input for cell burn-up calculation (when IC20=1)
*  31 1 1 0 0  0 0 0 0 0  10(0)   / IBC
*  31(1.790E-04)   / Power level  (MWt/cm)
*  0.10E+03  1.00E+03  2.50E+03  5.00E+03  7.50E+03  1.00E+04
*  1.25E+04  1.50E+04  1.75E+04  2.00E+04  2.25E+04  2.50E+04
*  2.75E+04  3.00E+04  3.25E+04  3.50E+04  3.75E+04  4.00E+04
*  4.25E+04  4.50E+04  4.75E+04  5.00E+04  5.25E+04  5.50E+04
*  5.75E+04  6.00E+04  6.25E+04  6.50E+04  6.75E+04  7.00E+04
*  7.25E+04  /  keff calculation is not done at the last step 
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
