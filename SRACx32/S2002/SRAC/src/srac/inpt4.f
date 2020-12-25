      SUBROUTINE INPT4 (NRMAX,     NMMAX,    NGMAX,    NNMAX,
     &       NNMAX1,    NNMAXR,    NK,       MTR,      RK,
     &       RN    ,    TAU,       VK,       LOC,      LSS,
     &       LGV   ,    XEC,       RRK,      RE,       RRN,
     &       H     ,    FLUX,      EX,       S         )
C
C * * * READ EXTRAPOLATION, INITIAL GUESS, FIXED SOURCE * * *
C
      DIMENSION  NK(NRMAX),MTR(NRMAX),RK(NRMAX),RN(NNMAX1),
     &           TAU(NNMAX1,8),LOC(NGMAX,NMMAX),   LSS(NGMAX,NMMAX),
     &           LGV(NGMAX,NMMAX),XEC(5000),
     &           RRK(NRMAX),RE(NGMAX),RRN(NNMAX1),H(NNMAXR,NGMAX),
     &           FLUX(NNMAX1,NGMAX),EX(NGMAX),S(NNMAXR,NGMAX),VK(NRMAX)
C
      COMMON /TUD1C/ II1(3),NGUP,NGKMAX,II6,IG,IBOUND,
     &               IGUESS,ID,ITMAX,ITMOUT,ITBG,LCMX,ITDM,IPT,
     &               EPS,EPS0,EPSG,RELCA,OVERX,FACTOR,XLAMD,BSQ1,
     &               IPTXEC,ITFLUX,IPTS,IDOPT,NXR,LCIK,LCNK,LCXR,
     &               LCRK,LCNN1,LCVOLR,LCMTM,LCMTR,DUMT(13),II(500)
C
      COMMON /MAINC/ IOPT(20),JNFSTL(2),FNFSTL(2),JNTHEL(2),FNTHEL(2)
     &    ,JNEFST(2),FNEFST(2),JNETHE(2),FNETHE(2),JNMACR(2),FNMACR(2)
     &    ,JNMCRS(2),FNMCRS(2),JNEMIC(2),FNEMIC(2),JNFLUX(2),FNFLUX(2)
     &   ,NEFL     ,NETL     ,NEF      ,NET      ,NERF     ,NERT
     &   ,NMAT     ,NETL1    ,BSQQ     ,NIN1     ,NIN2     ,NOUT1
     &   ,NOUT2,IT0,NEFL1    ,NEFL2    ,NEFL3    ,NEF1     ,NEF2
     &   ,NEF3     ,ISTEP    ,ISOIN,IFIN,IFOUT   ,ITYPE    ,DUMMY1(3)
     &   ,LCNEGF   ,LCNEGT   ,LCNECF   ,LCNECT   ,LCMTNM   ,LCNISO
     &   ,LCTEMP   ,LCXL     ,LCXCDC   ,LCLISO   ,LCIDNT   ,LCDN
     &   ,LCIRES   ,LCIXMC   ,DUMMY2(3),IRANG,ICF,DUM100
     &   ,CASEID(2),TITLE(18)
     &   ,III(880)
C
      DIMENSION XNOM(2)
C
    5 FORMAT(1H1,9X,20A4// 10X,'FIXED SOURCE ')
    6 FORMAT(10X,'NG=',I3/(10X,1P10E12.4))
C
C
C             OUTER BOUNDARY CONDISION
C               FLUX(N)-FLUX(N-1)
C            D* ------------------ -TAU*FLUX(N) = 0
C                 R(N)-R(N-1)
C IF  IBOUND =-1    TAU=-1.0 ZERO BOUNDRRY
C IF  IBOUND =0     TAU=0 REFLECTIVE
C IF  IBOUND =1     TAU CALCULATE =(1.0+ 0.710 * 3*IG*D/2/R(N)) /3/0.710
C IF  IBOUND =2     TAU READ IN AS CONSTANT
C IF  IBOUND =3     TAU READ IN AS ENERGY DEPENDENT VALUES
C
      FIG   = IG
      IF(IBOUND.LT.0) EXX = -1.0
      IF(IBOUND.EQ.0) EXX =  0.00
      IF(IBOUND.EQ.1) GO TO  20
      IF(IBOUND.EQ.2) EXX = XLAMD
      IF(IBOUND.GT.2) GO TO 120
C
      DO 10 NG = 1,NGMAX
   10 EX(NG)   = EXX
      GO TO 120
C
   20 R        = RN(NNMAX1)
      NM       = MTR(NRMAX)
      DO 25 NG = 1,NGMAX
      IDOP     = 7
      IF(IDOPT.EQ.2) IDOP = 8
      K        = LOC(NG,NM) + IDOP
   25 EX(NG)   = ( 1.0 + 0.710466*1.5*FIG*XEC(K)/R ) / 3.0 / 0.710466
C
C
C * * * FLUX GUESS * * *
C IF  IGUESS = 0   FLAT FLUX
C IF  IGUESS= N READ IN FINE STRUCTURE FROM FT0N
C IF  IGUESS=-1 READ IN FINE STRUCTURE FROM FT33
C
  120 IF(IGUESS.GT.0) GO TO 140
      IF(IGUESS.EQ.0) GO TO 125
C
      REWIND IFIN
      READ(IFIN) ((FLUX(I,J),I=1,NNMAX1),J=1,NGMAX)
      GO TO 300
C
  125 DO 130   NG=1,NGMAX
      DO 130   NN=1,NNMAX1
  130 FLUX(NN,NG)=1.0
      GO TO 300
C
  140 REWIND IGUESS
      READ (IGUESS)((FLUX(I,J),I=1,NNMAX1),J=1,NGMAX)
      GO TO 300
C
C * * * FISSION SOURCE GUESS * * *
C IF  IGUESS =0    FLAT DISTRIBUTION
C     IGUESS=N READ IN POINTWISE FLUX FROM FT0N
C
  300 IF(NGKMAX.EQ.0) GO TO 410
      NNR       = 0
      NN1       = 1
      DO 400 NR = 1,NRMAX
      NN1       = NN1    - 1
      M         = MTR(NR)
      NC        = NK(NR) + 1
      DO 380 NN = 1,NC
      NNR       = NNR    + 1
      NN1       = NN1    + 1
      RRN(NNR)  = 0.0
      DO 370 NG = 1,NGMAX
      L         = LOC(NG,M) + 4
      RRN(NNR)  = RRN(NNR)  + FLUX(NN1,NG)*XEC(L)
  370 CONTINUE
  380 CONTINUE
  400 CONTINUE
C
      WRITE(NOUT2,210) (RRN(NN),NN=1,NNMAXR)
      IF(ITYPE .EQ. 0) RETURN
C
  410 IF(IPTS  .NE. 0) WRITE(6, 5 )  CASEID,TITLE
      REWIND ISOIN
      READ(ISOIN)((S(I,J),I=1,NNMAXR),J=1,NGMAX)
      REWIND ISOIN
C
      DO 320 NG = 1,NGMAX
      IF(IPTS .NE. 0) WRITE(NOUT2,330) NG,(S(NNR,NG),NNR=1,NNMAXR)
      IF(IG.LT.2) GO TO 320
      NNR      = 0
      NN1      = 1
      RN(1)    = RN(2)
      DO 318 NR= 1,NRMAX
      NN1      = NN1    - 1
      NC       = NK(NR) + 1
      DO 318 NN= 1,NC
      NNR      = NNR    + 1
      NN1      = NN1    + 1
      S(NNR,NG)= S(NNR,NG)*RN(NN1)
  318 CONTINUE
      RN(1)    = 0.0
  320 CONTINUE
      RETURN
C
  210 FORMAT(///10X,'* * * FISSION SOURCE GUESS * * *'//(10X, 10E12.4))
  330 FORMAT('0NG=',I2,4X , 10E12.4/(10X ,10E12.4))
C     DEBUG SUBCHK
      END
