#!/bin/csh
#
##################################################################
#                                 
#  <<  run SRAC  >>
#                                   
#  by Keisuke OKUMURA (E-mail:okumura@mike.tokai.jaeri.go.jp)
#                                
##################################################################
#  sample problem CANDU : CANDU Type cell (IGT=10) by Pij
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
   set CASE = CANDU
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
DCA1
DCA CELL 1.2W/O 28 ROD CLUSTER 22.5CM PITCH
1 1 1 1 0  1 4 0 -2 1  0 0 0 0 2  2 2 0 0 0 / SRAC CONTROL
9.0E-4 / AXIAL BUCKLING
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
22  31  1   1 /
6(2) 6(4) 8(3) 2 4    / 66 GROUPS INTO 22 FINE GROUPS EC=1.2752EV
5 3 2 2 1 1 2 2 23(1) / 41 GROUPS INTO 31 FINE THERMAL
22                    / 22 INTO 1 FEW GROUP
31                    / 31 INTO 1 FEW GROUP

&IGT S  T  R  X   IBOUND NX NY NTPIN NAPIN NCELL EDITPIJ NGR NDA NDPIN
10  27 27 10  1     0    12  0  28     3    1      0      6   21   2
&IDIVP IBETM IPLOT
   2    45     1   /   PIJIN  CONTROL INTEGER
0 6(0) 6(0.)       /   ITERATION PARAMETERS
1 2  2 1  & FIRST  4 PIN ROD RING                      R-T 1
1 3  3 1  & SECOND 8 PIN ROD RING                      R-T 2
1 4  4 1  & THIRD 16 PIN ROD RING                      R-T 3
5 5 5 6 6 6 6     & H2O COOLANT                        R-T 4
7 8 9             & PRESSURE TUBE, GAP, CALANDRIA TUBE R-T 5
5(10)             / D2O MODERATOR                      R-T 6
10(1)               / X-R
2 1 1 1 3 3 2 4 2 5 / M-R
4 8 16              / NPIN ON RING
0.0 2.16  3.88 5.60 5.8 6.0 6.625 6.825 7.3 8.4 9.5 11.0 12.694  /RX
1.312   2.999  4.757 / RADII OF FUEL RING   RPP
0.0    3*90.0        & THETA FOR FIRST RING
22.5   7*45.0        & THETA FOR SECOND RING
11.25 15*22.5        / THETA FOR THIRD RING
0.0  0.720 0.835     / PIN ROD DIVISION RDP
6 2 1  / FOR PLOT GEOMETRY
5  / NMAT
FPINXF1X  0 3 300.  1.48  0. /  MAT 1 FUEL PELLET
XU050001  2 1 .0002806    /
XU080001  2 1 .022755     /
XO060001  0 0 .047387     /
CLADXCLX  0 2 300.  0.18  0. /  MAT 2 CLADDING
XAL70001  0 0 .05810      /
XMGN0001  0 0 .000172     /
LWTRXLWX  0 2 300.  0.    0. /  MAT 3 H2O
XH01H001  0 0 .06724
XO060001  0 0 .03336
AIRGXAGX  0 2 300.  0.    0. /  MAT 4 AIR GAP
XN040001  0 0 .0000396
XO060001  0 0 .0000108
HWTRXHWX  0 3 300.  0.    0. /  MAT 5 D2O
XH01H001  0 0 .0000368
XD02D001  0 0 .066568
XO060001  0 0 .03322


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
