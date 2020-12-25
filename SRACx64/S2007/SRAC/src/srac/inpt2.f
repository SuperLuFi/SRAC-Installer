      SUBROUTINE INPT2 (NRMAX,    NNMAX,    NNMAX1,   NNMAXR,      NK,
     &                  RK   ,       RN,       TAU,       VK,      VM)
C
      DIMENSION     NK(NRMAX),RK(NRMAX),RN(NNMAX1),VM(NMPT),
     &              TAU(NNMAX1,8),VK(NRMAX),NODE(2),IRANGE(3)
C
      COMMON /PDSPDS/BUF(540),IFLSW,FILENM(3),ECODE,TEMRY
      COMMON /TUD1C/ DUMY(6),IG,IBOUND,
     &               IGUESS,ID,ITMAX,ITMOUT,ITBG,LCMX,ITDM,IPT,
     &               EPS,EPS0,EPSG,RELCA,OVERX,FACTOR,XLAMD,BSQ1,
     &               IPTXEC,ITFLUX,IPTS,IDOPT,NXR,LCIK,LCNK,LCXR,
     &               LCRK,LCNN1,LCVOLR,LCMTM,LCMTR,LCVLMT,
     &               DUMT(12),II(500)
C
      COMMON /MAINC/ IOPT(20),JNFSTL(2),FNFSTL(2),JNTHEL(2),FNTHEL(2)
     &    ,JNEFST(2),FNEFST(2),JNETHE(2),FNETHE(2),JNMACR(2),FNMACR(2)
     &    ,JNMCRS(2),FNMCRS(2),JNEMIC(2),FNEMIC(2),JNFLUX(2),FNFLUX(2)
     &   ,NEFL     ,NETL     ,NEF      ,NET      ,NERF     ,NERT
     &   ,NMAT     ,NETL1    ,BSQB     ,NIN1     ,NIN2     ,NOUT1
     &   ,NOUT2,IT0,NEFL1    ,NEFL2    ,NEFL3    ,NEF1     ,NEF2
     &   ,NEF3     ,ISTEP    ,DUMMY1(7)
     &   ,LCNEGF   ,LCNEGT   ,LCNECF   ,LCNECT   ,LCMTNM   ,LCNISO
     &   ,LCTEMP   ,LCXL     ,LCXCDC   ,LCLISO   ,LCIDNT   ,LCDN
     &   ,LCIRES   ,LCIXMC   ,DUMMY2(3),IRANG,ICF,DUM100
     &   ,CASEID(2),TITLE(18)
     &   ,III(880)
C
      EQUIVALENCE (DUMY(2),NMPT)
C
      CHARACTER*4 IRANGE ,FILENM,NODE ,CASEID
C
      DATA IRANGE  /'FAST','THER','ALL '/
C
C    GEOM   SLAB      CYLINDER   SPHERE
C     IG      0          1         2
C     IG1     1          2         3
C     IG2     0          0         1
C     IG3     0          1         1
C
      FIG  = IG
      IG1  = IG+1
      IG2  = IG/2
      IG3  = IG-IG2
      NN   = 0
      RM   = 0.0
      RN(1)= 0.0
      DO 10 NR = 1,NRMAX
      DR       = ( RK(NR) - RM ) / FLOAT(NK(NR))
      N1       = NK(NR)
      DO  9  N = 1,N1
      NN       = NN + 1
    9 RN(NN+1) = RN(NN) + DR
      RM       = RK(NR)
   10 CONTINUE
C
C * *  TAU-,TAU+,MU-,MU+,* *
C
      LENG        =  NNMAX1*8
      CALL  CLEA(  TAU , LENG , 0.0 )
C
      DO 20    NN = 2,NNMAX1
      DR          = RN(NN)  - RN(NN-1)
      X           = FIG*( 2.00 - FIG )*DR*0.500
      TAU(NN,1)   = ( 1.0 - X/RN(NN)   )/DR
      IF(NN.EQ.2) GO TO 16
      TAU(NN-1,2) = ( 1.0 + X/RN(NN-1) )/DR
   16 CONTINUE
      X           = FIG*( 4.0-  FIG)*DR*0.08333333333
      TAU(NN,5)   = DR *( 1.0 - X/RN(NN) )*0.500
      IF(NN.EQ.2) GO TO 17
      TAU(NN-1,6) = DR*(1.0 + X/RN(NN-1) )*0.500
   17 CONTINUE
      TAU(NN,3)   = TAU(NN,5)*RN(NN)**IG3
      IF(NN.EQ.2) GO TO 18
      TAU(NN-1,4) = TAU(NN-1,6)*RN(NN-1)**IG3
   18 CONTINUE
      X           = FIG*(FIG-1.0)*0.500*DR
      TAU(NN,7)   = 1.00 - X/RN(NN)
      IF(NN.EQ.2) GO TO 20
      TAU(NN-1,8) = 1.00 + X/RN(NN-1)
   20 CONTINUE
      DR          = RN(2)-RN(1)
      TAU(1,2)    = (FIG+1.0)/DR
      TAU(1,4)    = 1.000/(FIG+1.0)*(DR/2.0)**IG1/DR**IG2
      TAU(1,6)    = DR*0.500
      IF(IG.EQ.2) TAU(2,1) = 0.00
      TAU(1,8)    =  1.00
      TAU(NNMAX1,1) = 1./(RN(NNMAX1)-RN(NNMAX))*(RN(NNMAX1)/RN(NNMAX))
     &   **IG2
      TAU(NNMAX1,2) = 0.0
      TAU(NNMAX1,4) = 0.0
      TAU(NNMAX1,6) = 0.0
      TAU(NNMAX1,8) = 0.0
      CALL CLEA (II(LCVLMT),NMPT,0.0 )
      DR       = 0.
      GCOEF    = 1.0
      IF(IG.EQ.1)  GCOEF = 3.141592
      IF(IG.EQ.2)  GOCEF = 4.000000*3.141592*0.33333333
C
      DO 50 NR = 1,NRMAX
CM    VK(NR)   = (RK(NR)**IG1-DR**IG1)/(FIG+1.0)
      VK(NR)   = (RK(NR)**IG1-DR**IG1)*GCOEF
      NM       = II(LCMTR+NR-1)
      VM(NM)   = VM(NM)+VK(NR)
      DR       = RK(NR)
   50 CONTINUE
C
*     WRITE(NOUT2,51) (VM(K),K=1,NMPT)
*  51 FORMAT(1H ,' ## VM ## ',1P10E12.5)
*     WRITE(NOUT2,52) (VK(K),K=1,NRMAX)
*  52 FORMAT(1H ,' ## VK ## ',1P10E12.5)
C
      FILENM(1)= 'FLUX'
      FILENM(2)= '    '
      FILENM(3)= '    '
      NODE(1)  = CASEID(1)
      NODE(2)  = 'XVOL'
      CALL PACKX(NODE(2),1,IRANGE(IRANG+1),1)
      CALL SEARCH(NODE,LGH,ISW)
      IF(ISW.EQ.0) CALL DELETE(NODE)
      CALL WRITE(NODE,VK,NRMAX)
      RETURN
C     DEBUG SUBCHK
      END
