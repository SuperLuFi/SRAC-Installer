#!/bin/csh
#
##################################################################
#                                 
#  <<  run SRAC  >>
#                                   
#  by Keisuke OKUMURA (E-mail:okumura@mike.tokai.jaeri.go.jp)
#                                
##################################################################
#  sample problem BWR : ABWR fuel assembly with MOX fuel
#
#  Change the include parameters in PIJPMINC :                  
#  PARAMETER ( MEMPIJ = 2 0000 , MXMESH = 300  , MXNTAB = 3000 )
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
   set CASE = BWR
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
UPIN
Unit MOX Fuel Pin
1 1 1 1 2   1 4 3 -2 1   0 0 0 0 2   0 -1 0 0 0 / SRAC Control
1.0E-15  / Buckling                                     
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
61 46 19 20              /  107g => 19+20=39g
61(1)                    /
46(1)                    /
10 10 10 7 8  2 2 12(1)  /
17(2) 4 4 4              /  
                                                                      
4 7 7 3 1  1 7 0 0 0  5 0 6 23 0  0 45 0   / Pij Control 
0 100 50 5 5 5 -1  0.0001  0.00001  0.001  1.0 10. 0.5  
1 1 1  2  3 3 3 / R-T
1 1 1           / X-R
1 2 3           / M-R
0.0 0.3054 0.4319 0.529  0.615  0.682 0.748 0.815  / RX 
3 / NMAT
MOX1X01X  0  8  900.0   1.058  0.0  / MOX Pellet
XU050009  2  0  4.40524E-05
XU080009  2  0  2.17045E-02
XPU90009  2  0  6.73856E-04
XPU00009  2  0  3.18053E-04
XPU10009  2  0  1.30771E-04
XPU20009  2  0  7.76602E-05
XAM10009  2  0  5.99868E-06
XO060009  0  0  4.59098E-02 
ZR21X02X  0  1  600.0   0.172  0.0  / Cladding for MOXl Pin
XZRN0008  0  0  4.32418E-02
H2O1X03X  0  2  600.0   0.0    0.0  / Moderator(42%Void)
XH01H008  0  0  2.96967E-02
XO060008  0  0  1.48483E-02
0 / PEACO
GDPN
3*3 Array with UO2-Gd2O3 Rod in Center (IGT=9)
1 1 1 1 2   1 4 3 -2 1   0 0 0 0 2   0 -1 0 0 0 / SRAC Control
1.0E-15  / Buckling
9 36 14 6 4   0 3 0 0 3   2 0 6 23 5   0 45 0  / Pij Control
0 100 50 5 5 5 -1  0.0001  0.00001  0.001  1.0 10. 0.5  
10 14 10 6 14  10 7 7 7 8  9 11 11 11 12  13 7 7 7 8
9 1 2 3 4  5 11 11 11 12  13 7 7 7 8  9  / T-S
1 1 1 2 3  3 4 5 6 6  4 5 6 6  /  R-T
1 2 3 4 4  4                   /  X-R
1 2 3 4 5  6                   /  M-R
0.0 3*1.63          /  RX
0.815 2*1.63        /  RPP
6(0.0 0.3054 0.4319 0.529  0.615  0.715)  / RDP
&15 2 1  / Plot Geometry
6 / NMAT
GDUOX04X  0  9  900.0   1.058  0.0  / 1: UO2-Gd2O3(4.5wt%)
XU050009  2  0  7.66040E-04
XU080009  2  0  2.11208E-02
XO060009  0  0  4.60776E-02
XGD40009  2  0  3.23186E-05
XGD50009  2  0  2.27770E-04
XGD60009  2  0  3.17031E-04
XGD70009  2  0  2.41621E-04
XGD80009  2  0  3.81668E-04
XGD00009  2  0  3.35498E-04
ZR22X05X  0  1  600.0   0.172  0.0  / 2: Cladding for Gd2O3-UO2
XZRN0008  0  0  4.32418E-02
H2O2X06X  0  2  600.0   0.0    0.0  / 3: Moderator around Gd2O3-UO2
XH01H008  0  0  2.96967E-02
XO060008  0  0  1.48483E-02
MOX2X07X  0  8  900.0   1.058  0.0  / 4: MOX Fuel
XU050009  2  0  4.40524E-05
XU080009  2  0  2.17045E-02
XPU90009  2  0  6.73856E-04
XPU00009  2  0  3.18053E-04
XPU10009  2  0  1.30771E-04
XPU20009  2  0  7.76602E-05
XAM10009  2  0  5.99868E-06
XO060009  0  0  4.59098E-02 
ZR23X08X  0  1  600.0   0.172  0.0  / 5: Cladding for UO2 fuel Pin
XZRN0008  0  0  4.32418E-02
H2O3X09X  0  2  600.0   0.0    0.0  / 6: Moderator around UO2 Pin
XH01H008  0  0  2.96967E-02
XO060008  0  0  1.48483E-02
0 / PEACO
CRIN
Control-Blade with B4C Powder (Rod In)
1 1 1 1 0   1 4 0 -2 1   0 0 0 0 2   0 -1 0 0 0 / SRAC Control
1.0E-15  / Buckling
16 15 12 9 6   1 1 13 1 0   3 0 10 43 2   1 90 0  / Pij Control
0 100 50 5 5 5 -1  0.0001  0.00001  0.001  1.0 10. 0.5  
3 4 5 6 7   8 9 10 10 11   11 12 12 1 2  / T-S
1 2 3 4 5   6 7 8 8 9   9 9    / R-T
1 1 1 1 2   3 4 5 6            / X-R                               
1 2 3 4 3   5 6 7 8            / M-R                                       
0.0 0.29   / RX
0.0 0.29 0.4 1.05 1.1525 1.48 8*0.815 / TY
2  / IXP
1  / IYP
0.0 0.21 0.28 / RDP
8 / NMAT
B4CPX0AX  0  3  600.0 0.0  0.0 / 1:B4C Pellet(Nat.B and 70%T.D)
XB000008  0  0  1.50776E-02
XB010008  0  0  6.12203E-02
XC020008  0  0  1.90745E-02
CLABX0BX  0  4  600.0 0.0  0.0 / 2:Cladding for Absorber
XCRN0008  0  0  1.55546E-02
XNIN0008  0  0  9.72490E-03
XMON0008  0  0  1.24036E-03 
XFEN0008  0  0  5.83534E-02
SATWX0CX  0  2  600.0 0.0  0.0 / 3:Moderator(Sat.Water)
XH01H008  0  0  4.94151E-02
XO060008  0  0  2.47075E-02
SHEAX0DX  0  4  600.0 0.0  0.0 / 4:Outer Sheath for Control Blade
XCRN0008  0  0  1.55546E-02
XNIN0008  0  0  9.72490E-03
XMON0008  0  0  1.24036E-03
XFEN0008  0  0  5.83534E-02
CHBXX0EX  0  4  600.0 0.0  0.0 / 5:Channel Box
XZRN0008  0  0  4.24851E-02
XFEN0008  0  0  1.05945E-04
XSNN0008  0  0  5.14929E-04
XCRN0008  0  0  7.58618E-05
H2O4X0FX  0  2  600.0 0.0  0.0 / 6:Moderator(42%Void) for Clearance
XH01H008  0  0  2.96967E-02
XO060008  0  0  1.48483E-02
UPINX01X  0  0  0.0  0.0  0.0  / 7:Homogenized Fuel near Channel Box
UPINX01X  0  0  0.0  0.0  0.0  / 8:Homogenized Fuel 
CROT                                                                   
Control-Blade with B4C Powder (Rod out)
1 1 1 1 0   1 4 0 -2 1   1 0 0 0 2   0 -1 0 0 0 / SRAC Control
1.0E-15  / Buckling
8 / NMAT                                                             
SATWX0CX  0  0  0.0  0.0  0.0  / 1:Moderator(Sat.Water):B4C Replaced
SATWX0CX  0  0  0.0  0.0  0.0  / 2:Moderator(Sat.Water):Clad Replaced
SATWX0CX  0  0  0.0  0.0  0.0  / 3:Moderator(Sat.Water)
SATWX0CX  0  0  0.0  0.0  0.0  / 4:Moderator(Sat.Water):Sheath Replaced
CHBXX0EX  0  0  0.0  0.0  0.0  / 5:Channel Box
H2O4X0FX  0  0  0.0  0.0  0.0  / 6:Moderator(42%Void) for Clearance
UPINX01X  0  0  0.0  0.0  0.0  / 7:Homogenized Fuel near Channel Box
UPINX01X  0  0  0.0  0.0  0.0  / 8:Homogenized Fuel
ASMI
8*8 ABWR Fuel Assembly with Control Blade by PIJ
1 0 0 1 0   1 0 0 0 1   0 1 0 0 2   0 1 0 0 0  / SRAC Control            
1.0E-15  / Buckling
16 649 219 219 1  1 25 25 12 0  2 0 6 23 2  1 45 0   / Pij Control
0 100 50 5 5 5 -1  0.0001  0.0001  0.001  1.0 10. 0.5
  28  29  30  30  31   1   1   2   2   3   3   4   4   5   5   6   6   7
   8  32  32  34  35  35  36
  29  37  38  38  39  40  41  42  42  43  43  44  44  45  45  46  46  47
  47  48  49  50  51  51  52
  30  38   9   9  10  10  10  11  11  12  12  13  13  14  14  15  15  16
  16  16  17  17  18  18  53 
  30  38   9  64  65  65  65  66  66  67  67  68  68  69  69  70  70  71
  71  72  73  74  75  18  53
  31  39  10  65  86  86  87  89  89  92  92  94  94  97  97 100 100 102
 102 102 105 106  76  19  54
   1  40  10  65  86  86  87  89  89  92  92  94  94  97  97 100 100 102
 102 102 105 106  76  19  54
   1  41  10  65  87  87  88  90  91  93  93  95  96  98  99 101 101 103
 104 104 107 108  77  19  54 
   2  42  11  66  89  89  90 109 110 174 175 112 113 116 117 178 179 120
 121 121 124 125  78  20  55
   2  42  11  66  89  89  91 110 111 176 177 114 115 118 119 180 181 122 
 123 123 126 125  78  20  55
   3  43  12  67  92  92  93 174 176 127 128 130 131 182 183 134 135 186 
 187 187 138 139  79  21  56
   3  43  12  67  92  92  93 175 177 128 129 132 133 184 185 136 137 188
 189 189 140 139  79  21  56
   4  44  13  68  94  94  95 112 114 130 132 198 199 201 202 190 191 141
 142 142 145 146  80  22  57
   4  44  13  68  94  94  96 113 115 131 133 199 200 203 204 192 193 143
 144 144 147 146  80  22  57
   5  45  14  69  97  97  98 116 118 182 184 201 203 205 206 148 149 194
 195 195 152 153  81  23  58
   5  45  14  69  97  97  99 117 119 183 185 202 204 206 207 150 151 196
 197 197 154 153  81  23  58
   6  46  15  70 100 100 101 178 180 134 136 190 192 148 150 155 156 158 
 159 159 162 163  82  24  59 
   6  46  15  70 100 100 101 179 181 135 137 191 193 149 151 156 157 160
 161 161 164 163  82  24  59
   7  47  16  71 102 102 103 120 122 186 188 141 143 194 196 158 160 165
 166 166 168 169  83  25  60 
   8  47  16  71 102 102 104 121 123 187 189 142 144 195 197 159 161 166
 167 167 170 169  83  25  60
  32  48  16  72 102 102 104 121 123 187 189 142 144 195 197 159 161 166 
 167 167 170 169  83  25  60
  33  49  17  73 105 105 107 124 126 138 140 145 147 152 154 162 164 168 
 170 170 171 172  84  26  61
  34  50  17  74 106 106 108 125 125 139 139 146 146 153 153 163 163 169
 169 169 172 173  84  26  61 
  35  51  18  75  76  76  77  78  78  79  79  80  80  81  81  82  82  83
  83  83  84  84  85  27  62
  35  51  18  18  19  19  19  20  20  21  21  22  22  23  23  24  24  25
  25  25  26  26  27  27  62
  36  52  53  53  54  54  54  55  55  56  56  57  57  58  58  59  59  60
  60  60  61  61  62  62  63
& 25 x 25 Grid 
         208 214                 209 215
 208 214                 210 216         211 217 
                                 212 218
         210 216                         213 219
 209 215         212 218
         211 217         213 219
& 12 Gd PIN /  T-S
219(1)      /  X-R
 8(5)   & Control Blade (Satulated Water when Control rod out)
19(7)   & Channel Box
 4(10)  & Central Structure of Control Blade
32(6)   & Flow Area out of Channel(Satulated Water)
22(8)   & Void Water inside Channel (next to Channel Box)
       23(1)  &  Homogenized Fuel (MOX)
15(1)   3(1)  &  MOX and Outer MOX
11(1)   3(1)
 4(1)   3(1)
 4(1)   3(1)
 7(1)   3(1)
 3(1)   3(1)
        3(1)
24(4)   & Void Water inside Channel (next to Gd Pin)
10(9)   & Water Rod
 6(2)   & Gd2O3-UO2 Fuel
 6(3)   & Cladding of Gd Pin   (End of M-R)
0.0    1*0.4   1*0.65  1*0.1025  1*0.3275  
       2.26 2.295 1*0.815       & 1st column fuel 
       10*0.815                 & 2nd - 6th column fuel
       1*0.815 12.7 12.89       & 7th column fuel
       2*0.815                  & 8th column fuel
       1*0.3275  1*0.1025  1*0.55   / RX
0.0    1*0.4   1*0.65  1*0.1025  1*0.3275  
       2.26 2.295 1*0.815       & 1st row fuel
       10*0.815                 & 2nd - 6th row fuel 
       1*0.815 12.7 12.89       & 7th row fuel 
       2*0.815                  & 8th row fuel
       1*0.3275  1*0.1025  1*0.55   / TY
11 17  9 15 19 17 11 19  9 13 11 15 / IXP
 9  9 11 11 11 13 15 15 17 17 19 19 / IYP
12(0.0 0.529 0.615) / RDP
10 / NMAT
UPINX01X  0  0  0.0  0.0  0.0  / 1:Fuel Region of MOX Pin
GDPNX01X  0  0  0.0  0.0  0.0  / 2:Fuel Region of Gd2O3-UO2 Pin 
GDPNX02X  0  0  0.0  0.0  0.0  / 3:Cladding for Gd2O3-UO2 Pin
GDPNX03X  0  0  0.0  0.0  0.0  / 4:Moderator(42%Void) near Gd Pin
CRINX01X  0  0  0.0  0.0  0.0  / 5:Control Rod Blade
CRINX02X  0  0  0.0  0.0  0.0  / 6:Wide Gap (Sat.Water)
CRINX03X  0  0  0.0  0.0  0.0  / 7:Channel Box
CRINX04X  0  0  0.0  0.0  0.0  / 8:Clearance Void Water
WTRDX0GX  0  3  600.0 0.0  0.0 / 9:Water Rod 
XZRN0008  0  0  3.64312E-03    /  Clad(8.425%)+(Water(42%void)(91.575%))
XH01H008  0  0  2.71948E-02
XO060008  0  0  1.35973E-02  
CSTRX0HX  0  4  600.0 0.0  0.0 / 10:Central Structure of Control Blade
XCRN0008  0  0  1.55546E-02
XNIN0008  0  0  9.72490E-03
XMON0008  0  0  1.24036E-03
XFEN0008  0  0  5.83534E-02
ASMO
8*8 ABWR Fuel Assembly without Control Blade by PIJ
1 0 0 1 0   1 0 0 0 1   1 1 0 0 2   0 1 0 0 0  / SRAC Control
1.0E-15  / Buckling   
10 / NMAT
UPINX01X  0  0  0.0  0.0  0.0  / 1:Fuel Region of MOX Pin
GDPNX01X  0  0  0.0  0.0  0.0  / 2:Fuel Region of Gd2O3-UO2 Pin
GDPNX02X  0  0  0.0  0.0  0.0  / 3:Cladding for Gd2O3-UO2 Pin
GDPNX03X  0  0  0.0  0.0  0.0  / 4:Moderator(42%Void) near Gd Pin
CROTX01X  0  0  0.0  0.0  0.0  / 5:Control Rod Blade Replaced by Sat.Water
CROTX02X  0  0  0.0  0.0  0.0  / 6:Wide Gap (Sat.Water)
CROTX03X  0  0  0.0  0.0  0.0  / 7:Channel Box
CROTX04X  0  0  0.0  0.0  0.0  / 8:Clearance Void Water
WTRDX0GX  0  0  0.0  0.0  0.0  / 9:Water Rod 
CROTX02X  0  0  0.0  0.0  0.0  / 10:Central Structure Replaced by Sat.Water


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
