C **********************************************************************
C          ANISN  A ONE DIMENSIONAL NEUTRON TRANSPORT CODE
C
C          GENERAL ANISOTROPIC SN
C                  ***         **
C
C
C          WRITTEN BY W W ENGLE
C
C          P(L) SCATTERING DEVELOPEMENT BY F R MYNATT
C
C          COMPUTING TECHNOLOGY CENTER     UNION CARBIDE CORP
C                       OAK RIDGE TENNESSEE
C
C          THE CODE NAME  ANISN  SUGGESTED BY W A RHOADES OF
C          ATOMICS INTERNATIONAL    CANOGA PARK CALIFORNIA
C
C
C          RECURSIVE RELATIONS FOR P-L SCATTERING COEFFICIENTS STOLEN
C          FROM DTF-IV, WRITTEN BY K D LATHROP OF LASL
C          LOS ALAMOS    NEW MEXICO
C
C
C   *** MAIN PROGRAM IS ALMOST INSIGNIFICANT
C
C   ID DEFINTION                                        CONDITION  DIMEN
C   1* CHI OR FISSION SPECTRUM                                       IGM
C   2* FISSION DENSITY                                      IFN=0     IM
C   3* FLUX                                                 IFN=1     IG
C
C   4* RADII BY INTERVAL BOUNDARIES                                 IM+1
C   5* VELOCITIES                                                    IGM
C   6* SN WEIGHTS                                                     MM
C   7* SN COSINES                                                     MM
C
C   8& ZONE NUMBERS BY INTERVAL                                       IM
C   9& MATERIAL NUMBERS BY ZONE                                      IZM
C
C  10& MIXTURE NO. IN MIXING TABLE                        MS.GT.0     MS
C  11& COMPONENT NO. IN MIXING TABLE                      MS.GT.0     MS
C  12* NO. DENSITIES IN MIXING TABLE                      MS.GT.0     MS
C
C  13& ID NO. FOR ELEMENTS FROM LIBRARY                  MTP.GT.0    MTP
C  14* CROSS SECTIONS                                    MCR.GT.0    MCG
C
C  15& INTEGER PARAMETERS                                             36
C  16* FLOATING PT. PARAMETERS                                        14
C
CC 17* DISTRIBUTED SOURCE                                IQM.EQ.1     IG
C  17* DISTRIBUTED SOURCE                                IQM.GT.1     IG
C  18* SHELL SOURCE                                      IPM.GT.0   IPMI
C
C  19& ORDER OF SCATTER BY ZONE                         ISCT.GT.0    IZM
C  20* RADIUS MODIFIERS BY ZONE                         IEVT.EQ.4    IZM
C
C  21* DENSITY FACTORS BY INTERVAL                      IDFM.EQ.1     IM
C
C  22& MATERIAL NUMBERS FOR ACTIVITIES                   ID3.GT.0    ID3
C  23& X-SEC TABLE POSITION FOR ACTIVITIES               ID3.GT.0    ID3
C
C  24& SN/DIFFUSION/HOMOG. CELL FOR EACH GROUP         IDAT2.GT.0    IGM
C
C  25* ALBEDO BY GROUP - RIGHT BOUNDARY                  IBR.EQ.3    IGM
C  26* ALBEDO BY GROUP - LEFT  BOUNDARY                  IBL.EQ.3    IGM
C
C  27& X-REGION NO. BY ZONE
C
C  34* P-L COEFFICIENTS                                 IXTR.EQ.1  JT*MM
C
C
C   PARAMETER DEFINITION
C   15& INTEGER PARAMETERS
C   1    ID  PROBLEM ID NUMBER
C   2   ITH  0=FORWARD   1=ADJOINT
C   3  ISCT MAXIMUM ORDER OF SCATTER IN ANY ZONE
C   4   ISN  ORDER OF ANGULAR QUADRATURE
C   5   IGE  1=SLAB   2=CYLINDER   3=SPHERE
C   6   IBL  LEFT B.C.  0=VACUUM  1=REFL.  2=PERIODIC  3=WHITE
C   7   IBR  RIGHT B.C.  SAME AS LEFT B.C.
C   8   IZM  NUMBER OF ZONES
C   9    IM  NUMBER OF INTERVALS
C  10  IEVT  0=FIXED SOURCE  1=K  2=ALPHA  3=CONC. SEARCH  4=ZONE WIDTH
C            SEARCH  5=RADIUS SEARCH  6=BUCKLING SEARCH
C  11   IGM  NUMBER OF GROUPS
C  12   IHT  POSITION OF SIGMA TOTAL
C  13   IHS  POSITION OF SIGMA GG
C  14   IHM  CROSS SECTION TABLE LENGTH
C  15    MS  MIXING TABLE LENGTH
C  16   MCR  NO. OF X-SEC SETS FROM CARDS
C  17   MTP  NO. OF X-SEC SETS FROM TAPE
C  18    MT  NO. OF MATERIALS
C  19  IDFM  0=NO EFFECT   1=ENTER 21*
C  20  IPVT  0=NO EFFECT   1=ENTER K(0) AS PV   2=ENTER ALPHA(0) AS PV
C  21   IQM  0=NO EFFECT   1=ENTER 17*
C  22   IPM  0=NO EFFECT   1=ENTER 18* FOR ONE INTERVAL   IM=ENTER 18*
C            FOR ALL INTERVALS
C  23   IPP  INTERVAL CONTAINING 18* IF IPM.EQ.1   0 IF IPM.NE.1
C  24   IIM  INNER ITERATION MAXIMUM
C  25  ID1   0=NO EFFECT  1=PRNT ANG FLUX  2=PNCH FLUX  3=BOTH
C  26   ID2  0=NO EFFECT  1=GRP IND. X-SEC TAPE    2=USE PREV. TAPE
C  27   ID3  0=NO EFFECT  N=PRINT N ACTIVITIES BY ZONE  ENTER 22&,23&
C  28   ID4  0=NO EFFECT  1=PRINT N ACTIVITIES BY INTERVAL
C  29   ICM  OUTER ITERATION MAXIMUM
C  30 IDAT1  0=ALL IN CORE  1=X-SEC,Q ON TAPE  2=FLUX,J ON TAPE ALSO
C  31 IDAT2  0=NO EFFECT  GT.0=ENTER 24&    USE 24& FOR IDAT2 OUTERS
C  32   IFG  X-SEC WTING-0=NO, 1=YES, 2=READ, 3= WRITE, 4= 2 AND 3
C  33  IFLU  0=LINEAR,STEP IF N.LT.0  1=LINEAR ONLY  2=STEP ONLY
C  34   IFN  0=ENTER 2*   1=ENTER 3*   2=PREV.CASE
C  35  IPRT  0=PRNT X-SEC     1=DO NOT PRNT X-SEC
C  36  IXTR  0=CALC. P-L COEFFICIENTS    1=READ IN (34*)
C
C   16* FLOATING POINT PARAMETERS
C   1    EV  FIRST GUESS FOR EIGENVALUE  SEARCH ONLY
C   2   EVM  EIGENVALUE MODIFIER         SEARCH ONLY
C   3   EPS  PRECISION TO BE ACHIEVED
C   4    BF  BUCKLING FACTOR
C   5    DY  CYLINDER OR PLANE HEIGHT FOR BUCKLING CORRECTION
C   6    DZ  PLANE DEPTH FOR BUCKLING CORRECTION
C   7  DFM1  TRANSVERSE DIMENSION FOR VOID STREAMING CORRECTION
C   8   XNF  NORMALIZATION FACTOR
C   9    PV  0.0, K(0), OR ALPHA(0) WHEN IPVT=0,1,OR 2
C  10   RYF  RELAXATION FACTOR FOR LAMBDA2                  0.5
C  11  XLAL POINT CONVERGENE CRITERION IF .GT.0
C  12  XLAH  UPPER LIMIT FOR ABS(1-LAMBDA1)  SEARCH ONLY    0.05
C  13   EQL  EIGENVALUE CHANGE EPSILON       SEARCH ONLY MAX(2*EPS,.001)
C  14  XNPM  NEW PARAMETER MODIFIER          SEARCH ONLY    0.75
C
C   27& FEW GROUP PARAMETERS
C   1  ICON  0=NO EFFECT  1=MICRO X-SEC  2=MACRO   MINUS IMPLIES CELL
C   2  IHTF  FEW GRP SIGMA TOTAL POSITION
C   3  IHSF FEW GRP SIGMA GG POSITION   MINUS IMPLIES REMOVE UPSCATTER
C   4  IHMF  FEW GRP TABLE LENGTH
C   5  IPUN  0=NO EFFECT   1=PUNCH WEIGHTED X-SEC
C
C   DATA ORDER                      CONDITION
C   1  TITLE CARD                    ALWAYS
C   2  PARAMETERS  15&,16*,T         ALWAYS
C   3  CROSS SECTIONS  13&,14*,T     ID2.EQ.0
C   4  FIXED SOURCE  17*,18*,T       IEVT.EQ.0 AND ID2.LT.2
C   5  START GUESS  2*,3*,T          IFN.LT.2
C   6 REST OF DATA T                 ALWAYS
C
C   ANISN  DATA DEFINITIONS
C      ARRAY ADDRESSES  (ARRAY NAME PRECEDED BY L)
C   LXKI  FISSION SPECTRUM
C    LFD  FISSION DENSITY
C    LXN  FLUX
C     LR  RADII
C    LVE  VELOCITIES
C     LW  SN WEIGHTS
C   LDSN  SN COSINES
C    LMA  ZONE NUMBERS BY INTERVAL
C    LMZ  MATERIAL ID NUMBERS BY ZONE
C    LMB  MIXTURE NO. IN MIXING TABLE
C    LMC  COMPONENT NO. IN MIXING TABLE
C   LXMD  NO. DENSITY  IN MIXING TABLE
C   LMTT  ID NUMBERS OF ELEMENTS FROM LIBRARY
C   LCRX  CROSS SECTIONS
C   LFIX  INTEGER PARAMETERS
C   LFLT  FLOATING PARAMETERS
C     LQ  DIST. SOURCE
C    LPA  SHELL SOURCE
C    LJ5  ORDER OF SCATTER BY ZONE
C    LRM  RADIUS MODIFIER BY ZONE
C    LDF  DENSITY FACTORS
C    LJ3  MATERIAL NUMBERS FOR ACTIVITIES
C    LJ4  POSITIONS FOR ACTIVITIES
C   LIGT  SN/DIFF./HOMOG. CELL INDICATORS
C   LART  ALBEDO - RT. BOUNDARY
C  LALFT  ALBEDO - LFT. BOUNDARY
C   LFGP  FEW GROUP PARAMETERS
C   LFGG  FEW GROUP NUMBERS BY GROUP  (ARRAY NAME IS IFGG)
C   LEND  DUMMY
C     LV  VOLUMES
C    LAA  AREAS
C    LWD  WEIGHT * COSINE
C    LMR  REFLECTIVE DIRECTION INDICIES
C   LPNC  P-L COEFFICIENTS
C    LXJ  CURRENTS
C    LCH  P(1) THRU P(ISCT) SIGMA GG BY INTERVAL FOR CURRENT GROUP, IIG
C    LCA  SIGMA ABS BY INTERVAL FOR CURRENT GROUP
C    LCF  NOT USED
C    LCT  SIGMA TOTAL BY INTERVAL FOR CURRENT GROUP
C    LCS  P(0) SIGMA GG BY INTERVAL FOR CURRENT GROUP
C   LTAB  ALPHA/V ABSORPTION
C   LXND  ANGULAR FLUX FOR CURRENT GROUP
C    LSA  P(1)THRU P(ISCT) SOURCE BY INTERVAL (DOES NOT INCLUDE SLF-SCT)
C   LSAT  P(1)THRU P(ISCT) SOURCE INCLUDING SELF-SCATTER
C   LRAV  INTERVAL MIDPOINTS
C    LRA  CURRENT RADII
C   LXNN  NEW FLUX FOR CURRENT GROUP
C   LXNE  EXTRAPOLATED ANGULAR FLUX BY INTERVAL
C   LXNR  TOTAL SOURCE - S833     USED FOR PERIODIC B.C. TREATMENT -DT
C   LXNA  NEW J FOR CURRENT GROUP
C    LSR  P(0) SOURCE BY INTERVAL (DOES NOT INCLUDE SLF-SCT)
C    LST  P(01) SOURCE INCLUDING SELF-SCATTER
C    LQG  FIXED SOURCE BY GROUP
C    LFG  FISSION SOURCE BY GROUP
C    LSG  UPSCATTER SCALE FACTOR BY GROUP
C   LXKE  MODIFIED FISSION SPECTRUM   (XKI/LAMBDA)
C   LXNI  DUMMY ARRAY TO READ BOUNDARY FLUXES WHEN IDAT1.EQ.2
C   LXNO  LINEAR-STEP MARKER  (ARRAY NAME IS NO)
C    LT3  ACTIVITIES BY ZONE
C    LT5  ACTIVITIES BY INTERVAL
C    LDA  MU*A INFLUX EQUATIONS
C    LDB  DA + DC
C    LDC  ALPHA/W IN FLUX EQUATIONS
C    LDS  MU*A(I) IN FLUX EQUATIONS
C     LB  BOUNDARY FLUXES
C   END OF ARRAYS
C   IGMP  GROUP INDEX FOR FLUX,B,J
C   IGMM  GROUP DIMENSION FOR X-SEC,Q
C   IIGG  GROUP INDEX FOR X-SEC,Q
C   NERR  DATA ERROR COUNTER
C   IMJT  IM*JT
C    IHG  IHP IF IDAT1.NE.0   IHP*IGM IF IDAT1.EQ.0
C    IMP  NUMBER OF INTERVALS CONTAINING FIXED SOURCE
C     MP  1 IF IPM.EQ.0   MM IF IPM.GT.0
C    NDS  NO. OF DOWNSCATTER GROUPS
C    NUS  NO. OF UPSCATTER GROUPS
C    SDG  TOTAL OUTSCATTER
C    SCG  TOTAL SELF-SCATTER
C     AG  TOTAL ABSORPTIONS
C  XNLGG  LEAKAGE FOR CURRENT GROUP
C   XNLG  TOTAL LEAKAGE
C    SNG  TOTAL IN-SCATTER + FISSION SOURCE + FIXED SOURCE
C    ALA  LAMBDA2   SCATTER RATIO BETWEEN ITERATIONS
C    ASR  PREVIOUS TOTAL SCATTER
C    EAM  SUM OF (FLUX-FLUX(PREV))*(SIG(T)-SIG(GG)) FOR ALL INTERVALS
C    EPG  GROUP EPSILON
C     EQ  D(EV)/D(LAMBDA1)  EIGENVALUE SLOPE
C     E1 THRU E11  FLOATING TEMPORARIES
C    E12 THRU E15  UPSCATTER SCALING
C    E16 THRU E18  FLOATING TEMPORARIES
C    E19 SAVE DY FOR BUCKLING SEARCH
C    E20 SAVE DZ FOR BUCKLING SEARCH
C    ESC MAXIMUM FLUX DEVIATION FOR CURRENT GRP BETWEEN INNER ITERATIONS
C    ESM SUM OF (FLUX-FLUX(PREV))*SIG(GG) FOR ALL INTERVALS
C    EVP PREVIOUS EIGENVALUE  SEARCH ONLY
C   EVPP PREVIOUS PREVIOUS EIGENVALUE
C    FTP PREVIOUS TOTAL FISSION SOURCE
C     IC ITERATIONCOUNTER (OUTER)
C   ICVT CONVERGENCE TRIGGER    =1 CONVERGED    =0 NOT CONVERGED
C    IGP IGM+1
C     IG IGM*IM
C    IHP  IHM+1 IF NUS.GT.0    IHM IF NUS.EQ.0
C    IIC INNER ITERATION COUNTER FOR CURRENT GROUP
C    IIG CURRENT GROUP NO. IN ITER     ELSEWHERE USED AS DO INDEX
C     IP IM+1
C    IZP IZM+1
C    I01 THRU I00 ARE INTEGER TEMPORARIES
C     JT NUMBER OF CURRENT ARRAYS
C     LC TOTAL INNER ITERATION COUNTER
C     MG IGM*MM
C     MI IM*MM
C     ML MCR+MTP
C     MM NO. OF ANGLES
C    NFN LAMBDA1 COMPUTE TRIGGER   =0 COMPUTE  =1 DO NOT
C   XITR SUM OF SIGMA-GG FOR ALL INTERVALS FOR CURRENT GROUP
C   XLAP LAMBDA1 ASSOCIATED WITH EVP
C  XLAPP LAMBDA1 ASSOCIATED WITH EVPP
C   XLAR PREVIOUS LAMBDA1
C    XLA LAMBDA1 - TOTAL SOURCE RATIO BETWEEN ITERATIONS
C   XNIO AVERAGE FLUX LEAVING SYSTEM AT RIGHT BOUNDARY FOR IBR=3
C   XNII AVERAGE FLUX LEAVING SYSTEM AT LEFT  BOUNDARY FOR IBL=3
C    ZZ1 TOTAL SOURCE FOR CURRENT GRP (NO SLF-SCT) FOR INNER ITER SCALE
C    ZZ2 TIME LIMIT   (NZ2 WHERE USED)
C    ZZ3 SUM OF WEIGHT*COSINE FOR MU.GT.0   USED TO COMPUTE XNIO,XNII
C    XNB NEUTRON BALANCE
C   XKEP SUM OF XKE ARRAY
C   XKIP SUM OF XKI ARRAY
C     IH TEMPORARY INDEX   USUALLY USED FOR X-SEC POSITION
C      I TEMPORARY INDEX   USUALLY USED FOR INTERVAL LOOP
C      K TEMPORARY INDEX
C      L TEMPORARY INDEX
C      M TEMPORARY INDEX   USUALLY USED FOR ANGLE LOOP
C      J TEMPORARY INDEX
C      N TEMPORARY INDEX
C     NN TEMPORARY INDEX   USUALLY USED FOR ISCT LOOP
C    ISV IDAT1 FOR PREVIOUS PROBLEM
C   LIM1 AVAILABLE DATA LOCATIONS
C  T(12) TITLE
C   NIN  INPUT TAPE
C   NOU  OUTPUT TAPE
C   NT1  FLUX AND CURRENT STORAGE TAPE
C   NT2  FLUX AND CURRENT STORAGE TAPE  - PREVIOUS ITERATION
C   NT3  CROSS SECTION AND SOURCE TAPE
C  NT4  SCRATCH TAPE FOR NORMALIZATION OF SOURCE AND X-SEC MIXING
C  NT5  INITIAL TIME
C   NT6  LIBRARY TAPE
C   NT7  SPECIALLY PREPARED GRP. INDEPENDENT X-SEC TAPE
C
C
      SUBROUTINE  ANISN2
     *  (NMAT,MTNAME,NGR,NFFL,IMX)
      COMMON /SKPBUF/ LOCATN,JUMP,LFAST,LSLOW,JMP1,JMP2,JMP3
C     COMMON/BULKBU/  D(1),LIM1,DUMY(40000)
      COMMON /SN1C/ D1(1),LIM1
      COMMON /WORK/ Z(1),LIM2
      COMMON /MAINC/ D(1000)
      EQUIVALENCE (D(101),CASEID(1)),(D(103),TITLE(1)),(D(65),NOUT2)
     *     ,(D(96),MEMORY)
      DIMENSION MTNAME(2,NMAT),IMX(1),TITLE(18),CASEID(2)
C     NCASE=0
   10 CONTINUE
      LOCATN=20000
      LFAST=500000
      LSLOW=500000
      LIM2=MEMORY
      WRITE(NOUT2,6000) CASEID,TITLE
 6000 FORMAT(1H1,10X,'** CASE ',2A4,' ** TITLE ',18A4,
     1       '  STEP - ANISN2 -')
C     LIM1=65000
C     IF(NCASE.NE.0) GO TO 100
      CALL CLEAR(0.0,Z(3),LIM2-2)
  100 CALL CONTRL
     *  (NMAT,IMX,MTNAME,NGR,NFFL )
  999 RETURN
      END