C             TWTRN2              LEVEL=1        DATE=81.11.14
C
C     MAIN PROGRAM OF TWOTRAN-II, THE EXPORT VERSION OF TWOTRAN FOR
C     XY, RZ, AND RT GEOMETRIES
C
      SUBROUTINE TWTRN2 (NMAT,MTNAME,NGR)
      DIMENSION MTNAME(2,NMAT)
      COMMON /TW1C/ DD(1),LIM1,IA(210)
      COMMON /WORK/AAA(1),LIM2,AA(1)
C
C
      COMMON /DEPLET/ AKEFF (50)
      COMMON  /MAINC/ IOPT(100)
C
      EQUIVALENCE  ( IBURN , IOPT(20) )
      EQUIVALENCE  ( ITYPE , IOPT(77) )
      EQUIVALENCE  ( I79   , IOPT(79) )
      EQUIVALENCE  ( MEMORY, IOPT(96) )
      EQUIVALENCE  ( EV    , IA  (37) )
C
      DIMENSION    D(212),A(132)
C
      EQUIVALENCE (D(1),DD(1)),(A(1),AAA(1))
      EQUIVALENCE (D(135),TIMBDP),(D(136),TIMSLD),(D(107),IGCDMP),
     &            (D(151),NEXTER),(D(157),NOUT),
     &            (D(159),NDUMP1),(D(160),NDUMP2)
C
      EQUIVALENCE (A(29),TIN),(A(50),TIMACC)
C
C     ASSIGN MAXIMUM LENGTH OF BLOCKS A, FWBGN1, AND IA
C
      LIM2=MEMORY-2
C
C     INITIALIZE UNITS
C
      CALL REED (NDUMP1,0,0.0,0,4)
      CALL REED (NDUMP2,0,0.0,0,4)
C
C     INITIALIZE DUMP AND ERROR INDICATORS
C
      TIMBDP=600.0
      CALL SECOND (TIMSLD)
      IGCDMP=0
C    NERROR=0
      NEXTER=0
C
C     PROCESS CASE INPUT
C
      CALL INPUTW (NMAT,MTNAME,NGR)
C
C     PROCESS COMPUTATIONAL SECTION
C
      CALL GRIND2
C
C     PROCESS FINAL PRINT AND EDIT SUMMARIES
C
      CALL OUTPT3
      T=TIN
      CALL SECOND (TIN)
      T=(TIN-T+TIMACC)/60.0
      WRITE (NOUT,110)T
C
      IF(IBURN.GT.0.AND.ITYPE.EQ.0) THEN
                                    AKEFF(I79+1) = EV
CDEL                                AKINF(I79+1) = EV
                                    ENDIF
C
C     RETURN FOR NEXT CASE
C
      RETURN
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C LOCATION OF PROBLEM VARIABLES IN THE IA ARRAY (NU MEANS NOT USED)
C
C INPUT INTEGERS
C
C       1.ITH      2.ISCT     3.ISN      4.IGM      5.IM       6.JM
C       7.IBL      8.IBR      9.IBB     10.IBT     11.IEVT    12.ISTART
C      13.MT      14.MIN     15.MS      16.IHT     17.IHS     18.IHM
C      19.IQOPT   20.IQAN    21.IQB     22.NU      23.IPVT    24.IANG
C      25.IMC     26.IITL    27.JMC     28.IRBM    29.IXM     30.IYM
C      31.IEDOPT  32.IGEOM   33.IQR     34.IQT     35.ISDF    36.NU
C
C INPUT FLOATING POINT
C
C      37.EV      38.EVM     39.PV      40.XLAL    41.XLAH    42.XLAX
C      43.EPSO    44.EPSI    45.EPSR    46.EPSX    47.NU      48.NORM
C      49.POD     50.BHGT
C
C COMMONLY USED INTEGERS
C
C      51.IUP     52.IHF     53.IHA     54.IHTR    55.IHNN    56.IMJM
C      57.MM      58.NM      59.NMQ     60.IP      61.JP      62.IGP
C      63.IJMM    64.IT      65.JT      66.ITJT    67.ITMM    68.JTMM
C      69.NMIJ    70.NMM     71.ISPANC  72.IHMT    73.ISPANF  74.ISCP
C      75.IMJP    76.IPJM    77.ITP     78.JTP     79.ICLIM
C
C     FIRST WORD ADDRESSES OF ARRAYS ( FOR X READ R, FOR Y READ Z OR T )
C
C   80.LIHX  IHX(IM)          NUMBER OF X FINE MESH INTERVALS PER COARSE
C                             INTERVAL
C   82.LC    C(IHM,MT)        CROSS SECTIONS
C   83.LA1   A1(ITP)          A4(I+1)+A4(I)
C   84.LA2   A2(ITP)          A4(I+1)-A4(I)
C   85.LA3   A3(ITP)          Y DIRECTION SURFACE AREA
C   86.LA4   A4(ITP)          X DIRECTION SURFACE AREA=A4*YH
C   87.LA5   A5(ITP)          IN ALL GEOMETRIES VOLUME=A5*YH
C   88.LQ    Q(NM,IT,JT)      INPUT DISTRIBUTED SOURCES (CONDITIONAL)
C   89.LQR1  QR1(JT,MM)       INPUT RIGHT BOUNDARY SOURCES (CONDITIONAL)
C   90.LQR2  QR2(JT,MM)       INPUT RIGHT BOUNDARY SOURCES (CONDITIONAL)
C   91.LQT1  QT1(IT,MM)       INPUT TOP BOUNDARY SOURCES (CONDITIONAL)
C   92.LQT2  QT2(IT,MM)       INPUT TOP BOUNDARY SOURCES (CONDITIONAL)
C   93.LBR1  BR1(JT,MM)       RIGHT BOUNDARY FLUX (IN-DOWN AND OUT-DOWN)
C   94.LBR2  BR2(JT,MM)       RIGHT BOUNDARY FLUX (UP-IN AND UP-OUT)
C   95.LBT1  BT1(IT,MM)       TOP BOUNDARY FLUX (IN-DOWN AND IN-UP)
C   96.LBT2  BT2(IT,MM)       TOP BOUNDARY FLUX (OUT-DOWN AND OUT-UP)
C   97.LXDF XDF(IT)           RADIAL FINE MESH DENSITY FACTOR
C   98.LYDF  YDF(JT)          AXIAL FINE MESH DENSITY FACTOR
C   99.LIHXC IHXC(IMC)        SAME AS IHX, BUT FOR SEPARATE MAT. MESH
C  100.LIHYC IHYC(JMC)        SAME AS IHY, BUT FOR SEPARATE MAT. MESH
C  101.LDXC  IDXC(IT)         SAME AS IDX, BUT FOR SEPARATE MAT. MESH
C  102.LDYC  IDYC(JT)         SAME AS IDY, BUT FOR SEPARATE MAT. MESH
C  103.LDYAC IDYAC(JT)        SAME AS IDYA, BUT FOR SEPARATE MAT. MESH
C  105.LFL   FLUX(NM,IT,JT)   FLUX COMPONENTS
C  106.LFLA FLUXA(IT,JT)      FLUX FROM PREVIOUS INNER ITERATION
C  108.LFIS  FISS(IT,JT)      SAME ORIGIN AS FISSION SOURCE(NOT USED)
C  109.LFISA FISSA(IT,JT)     FISSION SOURCE
C  111.LDC   IDCS(IMJM)       CROSS SECTION ZONE IDENTIFICATION INTEGERS
C  112.LXR   XRAD(IP)         INPUT X COARSE MESH BOUNDARIES
C  113.LYR   YRAD(JP)         INPUT Y COARSE MESH BOUNDARIES
C  114.LDX   IDX(IT)          X DIRECTION INDICATORS--WHICH COARSE MESH
C                             IS A FINE X MESH INTERVAL IN
C  115.LDY   IDY(JT)          Y DIRECTION ZONE INDEX MULTIPLES
C  116.LDYA  IDYA(JT)         Y DIRECTION INDICATORS--WHICH COARSE MESH
C                             IS A FINE Y MESH INTERVAL IN
C  117.LXH   XH(IM)           FINE MESH X SPACING
C  118.LYH   YH(JM)           FINE MESH Y SPACING
C  119.LW    WGT(MM)          DIRECTION WEIGHTS
C  120.LCM   COSMU(MM)        X DIRECTION COSINES
C  121.LCE   COSETA(MM)       Y DIRECTION COSINES
C  122.LWM   WMU(MM)          PRODUCT WGT*COSMU
C  123.LWE   WETA(MM)         PRODUCT WGT*COSETA
C  124.LP1   P1(NM,MM)        SPERICAL HARMONIC FUNCTIONS IN DOWN SWEEP
C  125.LP2   P2(NM,MM)        SPERICAL HARMONIC FUNCTIONS OUT DOWN SWEEP
C  126.LP3   P3(NM,MM)        SPERICAL HARMONIC FUNCTIONS IN UP SWEEP
C  127.LP4   P4(NM,MM)        SPERICAL HARMONIC FUNCTIONS OUT UP SWEEP
C  128.LMN   MIXNUM(MS)       MIXTURE NUMBERS (CONDITIONAL INPUT)
C  129.LMC   MIXCOM(MS)       MIXTURE INSTRUCTIONS (CONDITIONAL INPUT)
C  130.LMD   MIXDEN(MS)       MIXTURE DENSITIES (CONDITIONAL INPUT)
C  131.LF    F(IM,JM)         COARSE MESH REBALANCE FACTORS
C  132.LFU   FU(IM,JP)        COARSE MESH UPWARD PARTIAL CURRENT
C  133.LFD   FD(IM,JP)        COARSE MESH DOWNWARD PARTIAL CURRENT
C  134.LFR   FR(IP,JM)        COARSE MESH RIGHTWARD PARTIAL CURRENT
C  135.LFLL  FL(IP,JM)        COARSE MESH LEFTWARD PARTIAL CURRENT
C  136.LAB   AB(IM,JM)        COARSE MESH ABSORPTION REMOVAL RATE
C  137.LQQ   QQ(IM,JM)        COARSE MESH SOURCE
C  138.LQQG  QQG(IM,JM)       COARSE MESH SOURCE OVER ALL GROUPS
C  139.LFUT  FUT(IM,JP)       SUM OF FU OVER ALL GROUPS
C  140.LFDT  FDT(IM,JP)       SUM OF FD OVER ALL GROUPS
C  141.LFRT  FRT(IP,JM)       SUM OF FR OVER ALL GROUPS
C  142.LFLT  FLT(IP,JM)       SUM OF FL OVER ALL GROUPS
C  143.LABT  ABT(IM,JM)       SUM OF ABSORPTION OVER ALL GROUPS
C  144.LHA   HA(IM)           USED IN REBAL FOR INVERSION
C  145.LGA   GA(IM)           USED IN REBAL FOR INVERSION
C  146.LQG   QG(IGP)          SPACE INTEGRAL OF Q
C  147.LFG   FG(IGP)          SPACE INTEGRAL OF FISSA
C  148.LSIN  SIN(IGP)         GROUP INSCATTER SOURCE
C  149.LSS   SS(IGP)          GROUP SELFSCATTER SOURCE
C  150.LSOUT SOUT(IGP)        GROUP OUTSCATTER SOURCE
C  151.LHL   HL(IGP)          HORIZONTAL LEAKAGE
C  152.LVL   VL(IGP)          VERTICAL LEAKAGE
C  153.LTL   TL(IGP)          TOTAL LEAKAGE
C  154.LNL   NL(IGP)          NET LEAKAGE
C  155.LRL   RL(IGP)          RIGHT LEAKAGE
C  156.LABG  ABG(IGP)         GROUP INTEGRAL OF ABSORPTION RATE
C  157.LBAL  BAL(IGP)         GROUP INTEGRAL OF NEUTRON BALANCE
C  158.LCHI  CHI(IGP)         INPUT FISSION SPECTRUM
C  159.LCHIA CHIA(IGP)        FISSION SPECTRUM USED IN CALCULATION
C  160.LVEL  VEL(IGP)         GROUP VELOCITIES
C  161.LYM   YM(JM)           Y DIRECTION RADICAL MODIFIERS (IEVT=4 ONLY
C  162.LXM   XM(IM)           X DIRECTION RADICAL MODIFIERS (IEVT=4 ONLY
C  163.LXRA  XRADA(IP)        MODIFIED COARSE MESH X BOUNDARIES
C  164.LYRA  YRADA(JP)        MODIFIED COARSE MESH Y BOUNDARIES
C  165.LSOU  SOURCE(NM,ITJT)  TOTAL SOURCE IN A GROUP
C  166.LANF  ANF(ITP,MM,2)    HORIZONTAL ANGULAR FLUX (IANG.GT.0 /ONLY)
C  167.LAA   AAJ(MT)          EFFECTIVE ABSORPTION
C  168.LB1   B1(JT)           ONE OVER DELTA Y USED TO  MULTIPLY ALL
C                             AREAS AND VOLUMES IN MAJOR RECURSION
C                             FORMULA
C  169.LAL1  AL1(MM)          ALPHA COEFFICIENT (M+1/2) / WGT
C  170.LAL2  AL2(MM)          ALPHA COEFFICIENT (M-1/2) / WGT
C  171.LALF  ALFL(NN,IT)      ALPHA FLUX DUE TO CURVATURE STREAMING
C  172.LQB1  QB1(IT,MM)       INPUT BOTTOM BOUNDARY FLUX (IN-UP)
C  173.LQB2  QB2(IT,MM)       INPUT BOTTOM BOUNDARY FLUX (OUT-UP)
C  174.LCTOT CTOT(IT,JT)      EFFECTIVE TOTAL CROSS SECTION
C
C  COMMONLY USED VARIABLES
C
C     175.JCONV  176.TN2N
C     177.XLAPP  178.XLAP   179.ICNT   180.E2     181.E1     182.EVPP
C     183.EVP    184.E4     185.NGO    186.ALAR   187.MESH   188.IITNO
C     189.TS     190.G      191.ICONV  192.NGOTO  193.E3     194.EVS
C     195.IITOT  196.ALA    197.TIN               198.FTP    199.IFN
C     200.OITNO  201.ZZ     202.BB     203.CC     204.DD     205.T
C     206.S      207.CT     208.SUMMU  209.SUMETA 210.NN     211.AA
C     212.TI     213.TJ     214.TM     215.NU     216.ERR    217.NU
C  220.LBT3  BT3(IT,MM)       TOP BOUNDARY FLUX ( PERIODIC BC ONLY )
C  223.LBT4  BT4(IT,MM)       TOP BOUNDARY FLUX ( PERIODIC BC ONLY )
C     225.NLIMIT 226.IFLAG
C     246.IANGPR 247.TIMACC 248.MCRRDS 249.NOSGUP 250.IOLYCS
C
C
  110 FORMAT (1H0//////'0***** TOTAL EXECUTION TIME IN MINUTES = ',
     &1PE12.4)
CKH   STOP
      END
