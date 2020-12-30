C             TUD2                LEVEL=15       DATE=84.05.21
      SUBROUTINE TUD2( NG ,MATNM)
C
C  * * * MULTI GROUP DIFFUSION CALCULATION BY S.O.R. METHOD * * *
C * *       BY TSUTIHASHI JAERI
C
C  *  *  NR         NUMBER OF REGION
C  *  *  NMP        NUMBER OF MATERIAL REGION
C  *  *  NMAT       NUMBER OF MATERIAL
C  *  *  NG         NUMBER OF ENERGY GROUP
C  *  *  NGUP       HIGHEST NUMBER OF GROUP INCLUDING UPSCATTER SOURCE
C  *  *  NGK        LOWEST NUMBER OF GROUP OF FISSION SPECTRUM
C  *  *  NN         NUMBER OF MESH INTERVAL FOR FLUX
C  *  *  NN1        NUMBER OF MESH POINT    FOR FLUX
C  *  *  NNR        NUMBER OF MESH POINT    FOR REACTION RATE
C  *  *  NK(NR)     NUMBER OF MESH INTERVAL IN REGION K
C  *  *  IK(NR)     MATERIAL IDENTIFICATION OF REGION K
C  *  *  RK(NR)     OUTER RADIUS OF REGION K
C  *  *  RN(NN)     OUTER RADIUS OF MESH INTERVAL NN
C  *  *  TAU(NN1,4) DIFFERENCE EQUATION PARAMETERS OF MESH POINT NN1
C  *  *  BSQ(NR)    TRANSVERS BUCKIING OF REGION K
C  *  *  LOC(NG,NMAT) FIRST LOCATION OF X-SECTION VECTOR OF GROUP NG
C  *  *                                                OF MATERIAL NM
C  *  *  XEC        X-SECTION ARRAY
C  *  *  LSS(NG,NMAT) POSITION OF SELF-SCATTER X-SECTION IN GROUP VECTOR
C  *  *  LGV(NG,NMAT) LENGTH OF GROUP VECTOR
C  *  *  RRK(NR)    REACTION RATE OF REGION K
C  *  *  RE(NG)     REACTION RATE OF ENERGY GROUP NG
C  *  *  RRN(NNR)   REACTION RATE OF MESH INTERVAL
C  *  *  H(NNR,NG)  FINE STRUCTURE OF REACTION RATE
C  *  *  FLUX(NN1,NG) NEUTRON FLUX
C  *  *  EX(NG)     EXTRAPORATED LENGTH OF GROUP NG
C  *  *  S(NNR,NGS) EXTERNAL SOURCE DISTRIBUTION
C  *  *  VK(NR)     VOLUME OF REGION K
C  *  *  VOLM(NMT)  VOLUME OF MATERIAL NMT
C  *  *  IG         GEOMETRY  =0 SLAB   =1 CYLINDER    = 2    SPHERE
C  *  *  IBOUND     OUTER BOUNDARY CONDITION INDEX
C  *  *  IGUESS     FLUX OR FISSION SOURCE INITIAL GUESS INDEX
C  *  *  ID         DIFFUSION PARAMETER INPUT INDEX
C  *  *  IBS        TRANSVERSE BUCKLING INPUT INDEX
C  *  *  LGXEC      LENGTH OF X-SECTION ARRAY
C  *  *  FIG        GEOMETRY INDEX (FLOATING)
C
      DIMENSION     I(60000),AA(1000),MATNM(2,1)
C
      COMMON /WORK / A(60000)
      COMMON /TUD1C/ NR, NMP      ,NGMAX,NGUP  ,NGK,NN,IG,IBOUND,
     &               IGUESS,ID,ITMAX,ITMOUT,ITBG,LCMX,ITDM,IPT,
     &               EPSI,EPSO,EPSG,RELCA,OVERX,FACTOR,XLAMD,BSQ1,
     &               IPTXEC,ITFLUX,IPTS,IDOPT,NXR,LCIK,LCNK,LCXR,
     &               LCRK,LCNN1,LCVOLR,LCMTM,LCMTR,LCVLMT,
     &               DUMT(12),II(1000)
C
      COMMON /MAINC/ IOPT(20),JNFSTL(2),FNFSTL(2),JNTHEL(2),FNTHEL(2)
     &    ,JNEFST(2),FNEFST(2),JNETHE(2),FNETHE(2),JNMACR(2),FNMACR(2)
     &    ,JNMCRS(2),FNMCRS(2),JNEMIC(2),FNEMIC(2),JNFLUX(2),FNFLUX(2)
     &   ,NEFL     ,NETL     ,NEF      ,NET      ,NERF     ,NERT
     &   ,NMAT     ,NETL1    ,BSQ      ,NIN1     ,NIN2     ,NOUT1
     &   ,NOUT2,IT0,NEFL1    ,NEFL2    ,NEFL3    ,NEF1     ,NEF2
     &   ,NEF3     ,ISTEP    ,NSOIN    ,NFIN,NFOUT,ITYPE   ,DUMMY1(3)
     &   ,LCNEGF   ,LCNEGT   ,LCNECF   ,LCNECT   ,LCMTNM   ,LCNISO
     &   ,LCTEMP   ,LCXL     ,LCXCDC   ,LCLISO   ,LCIDNT   ,LCDN
     &   ,LCIRES   ,LCIXMC   ,DUMMY2(3),IRANG    ,ICF      ,DUM100
     &   ,CASEID(2),TITLE(18)
     &   ,III(880)
C
      EQUIVALENCE (AA(1),II(1)),(A(1),I(1)),(DUMMY2(2),MXDI)
C
C  ** START OF PROCESS
C
      WRITE(NOUT2,6000) CASEID,TITLE
C
 6000 FORMAT(1H1,'** CASE ',2A4,' ** TITLE ',18A4,' STEP - TUD -')
      WRITE(NOUT2,1)
     &  NR,  IG,       IBOUND,   IGUESS,
     &              IPTXEC,   ITFLUX,   IPTS ,IDOPT,NXR
    1 FORMAT(1H1//10X,'TUD CONTROL DATA  '      /
     &10X,'NUMBER OF REGIONS                                  ',I3/
     &10X,'GEOMETRY(0,1,2)(SLAB,CYLINDER,SPHERE)              ',I3/
     &10X,'BOUNDARY CONDITION (-1,0,1,2)(ZERO,REFLECTIVE,     '  /
     &10X,'L.D. CALC,L.D. INPUT CONSTANT)                     ',I3/
     &10X,'GUESS (0,1) (FLAT,FINE STRUCTURE)                  ',I3/
     &10X,'CROSSSECTION PRINT (0,1) (SKIP,PRINT)              ',I3/
     &10X,'FLUX EDIT (0+1+4)(NONE+PRINT+FT33)                 ',I3/
     &10X,'FIXED SOURCE PRINT (0,1)(SKIP,PRINT)               ',I3/
     &10X,'D1 OR D2 USE OPTION (1,2)(D1,D2)                   ',I3/
     &10X,'NUMBER OF X-REGION                                 ',I3)
C
      IF (EPSI.GT.0) GO TO 40
      EPSI  = 1.E-4
      EPSO  = 1.E-5
      RELCA = 1.2
      EPSG  = 0.01
      OVERX = 100.
      FACTOR= 1.0
C
   40 CONTINUE
      IF(ITMAX.GT.0) GO TO 50
      ITBG  =   5
      LCMX  =   5
      ITDM  =   5
      IPT   =  -1
      ITMAX = 100
      ITMOUT=   1
      IF(IRANG.NE.1) ITMAX  = 10
      IF(IRANG.NE.1) ITMOUT = 40
C
   50 CONTINUE
      WRITE(NOUT2,13) ITMAX,ITMOUT,ITBG,LCMX,ITDM,IPT
C
   13 FORMAT(/10X,'ITERATION PARAMETERS  '
     &       /10X,'MAX OF INNER ITERATIONS                      ',I5
     &       /10X,'MAX OF POWER ITERATIONS                      ',I5
     &       /10X,'EARLIST EXTRAPOLATION               ',I15
     &       /10X,'NUMBER OF ITERATIONS TESTED         ',I15
     &       /10X,'MINIMUM DELAY                       ',I15
     &       /10X,'MONITOR PRINT(0,1)(SKIP,PRINT)      ',I15
     &       )
      WRITE(NOUT2,14) EPSI,EPSO,EPSG,RELCA,OVERX,FACTOR
   14 FORMAT(
     &       /10X,'CONVERGENCE CRITERION INNER         ',E15.5
     &       /10X,'CONVERGENCE CRITERION POWER         ',E15.5
     &       /10X,'EXTRAPOLATION CRITERION             ',E15.5
     &       /10X,'OVER-RELAXATION (INITIAL)           ',E15.5
     &       /10X,'MAX EXTRAPOLATION                   ',E15.5
     &       /10X,'BASE FACTOR OF OVER-RELAXATION      ',E15.5
     &    )
   15 FORMAT(
     &10X,'NUMBER OF ENERGY GROUPS                            ',I3)
   16 FORMAT(5X,' REG NO.     NBR MESH    MAT NO.     X-REG NO.  '
     &         ,' OUT RADIUS  REG VOL  ')
   17 FORMAT(1X,4I12,4X,2E12.5)
   18 FORMAT(10X,'TRANSVERSE BUCKLING= ',E12.5)
   19 FORMAT(10X,'EXTRAPOLATED DISTANCE= ',E12.5)
C
      NGMAX     = NG
      ICRN     = 1
      NN1      = NN   + 1
      NNR      = NN   + NR
      ICTA     = ICRN + NN1
      ICEX     = ICTA + 8*NN1
C
      CALL INPT2(
     &    NR,       NN,      NN1,    NNR,   II(LCNK),    AA(LCRK),
     &    A(ICRN),  A(ICTA), II(LCVOLR) ,   II(LCVLMT) )
C
      WRITE(NOUT2,15) NG
      WRITE(NOUT2,16)
      DO  100 N = 1,NR
      WRITE (NOUT2,17)  N,
     &II(LCNK+N-1),II(LCIK+N-1),II(LCXR+N-1),AA(LCRK+N-1),AA(LCVOLR+N-1)
  100 CONTINUE
      WRITE(NOUT2,18) BSQ1
      IF(IBOUND.EQ.2) WRITE(NOUT2,19) XLAMD
C
C
      ICLOC    = ICEX  +  NG
      ICLSS    = ICLOC + NG*NMP
      ICLGV    = ICLSS + NG*NMP
      ICXEC    = ICLGV + NG*NMP
C
      LENMAX   = NG*NMP*(10+NG)
      LAST     = ICXEC + LENMAX - 1
      IF(LAST.GT.MXDI) LAST = MXDI
      LENMAX   = LAST - ICXEC + 1
      WRITE(6,*) ' * ICXEC LAST MXDI LENMAX * ',ICXEC,LAST,MXDI,LENMAX
      CALL CLEA ( A(ICXEC) , LENMAX , 0.0 )
      CALL INPT3(NG,II(LCMTM),I(ICLOC),I(ICLSS),I(ICLGV),
     &              A(ICXEC),MATNM,A(ICXEC),LGXEC)
C
      ICRRK    = ICXEC  + LGXEC
      ICRE     = ICRRK  + NR
      ICRRN    = ICRE   + NG
      ICH      = ICRRN  + NNR
      ICFLUX   = ICH    + NNR*NG
      ICS      = ICFLUX + NN1*NG
      ICD1     = ICS    + NNR*NG
      ICD2     = ICD1   + NNR
      ICD3     = ICD2   + NNR
      ICD4     = ICD3   + NR
CFREE K        = ICD4   + 6*NN1
      KK       = ICD4   + 6*NN1
      LREBS    = KK
      LREBA    = LREBS  + NG*NG
      LREBQ    = LREBA  + NG
      LREBR    = LREBQ  + NG
      LREBF    = LREBR  + NG
      LREBT    = LREBF  + NG
      K        = LREBT  + NNR*NG
CEND
      IF( K.GT.MXDI) GO TO 999
C
      WRITE(NOUT1,900) K,MXDI
      WRITE(NOUT2,900) K,MXDI
C
      CALL INPT4(   NR,      NMP,       NG,       NN,
     &    NN1,      NNR,    II(LCNK)  , II(LCMTR),AA(LCRK),
     &    A(ICRN) ,A(ICTA) ,II(LCVOLR),I(ICLOC)  ,I(ICLSS),I(ICLGV),
     &    A(ICXEC),A(ICRRK), A(ICRE)  ,A(ICRRN)  ,A(ICH)  ,A(ICFLUX),
     &    A(ICEX) ,A(ICS)   )
C
      CALL ITUD (    NR,      NMP,       NG,       NN,
     &    NN1,      NNR,     II(LCNK), II(LCMTR), AA(LCRK),
     &    A(ICRN) ,A(ICTA) ,II(LCVOLR),I(ICLOC),I(ICLSS),I(ICLGV),
     &    A(ICXEC),A(ICRRK),A(ICRE) ,  A(ICRRN),A(ICH)  ,A(ICFLUX),
     &    A(ICEX) ,A(ICS)  ,A(ICD1) ,  A(ICD2) ,A(ICD3) ,A(ICD4)
CADD
     &   ,A(LREBS),A(LREBA),A(LREBQ),  A(LREBR),A(LREBF),A(LREBT)   )
CTFREE
      CALL OUTPT2( NR      ,NMP       ,NG       ,NN       ,
     &    NN1,     NNR     ,II(LCNK)  ,II(LCMTR), AA(LCRK),
     &    A(ICRN) ,A(ICTA) ,II(LCVOLR),I(ICLOC) ,I(ICLSS) ,I(ICLGV),
     &    A(ICXEC),A(ICRRK),A(ICRE)   ,A(ICRRN) ,A(ICH)   ,A(ICFLUX),
     &    A(ICEX) ,A(ICS)   )
      RETURN
C
  999 WRITE(NOUT1,900) K,MXDI
      WRITE(NOUT2,900) K,MXDI
      STOP
  900 FORMAT('0         STORAGE USED   ',I5,'  FROM  ',I5,' STEP  TUD')
C     DEBUG SUBCHK
      END
