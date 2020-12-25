CINFX-VP-093 ***CITATION*** INITIAL FLUX FOR 1,2-D/ CF-EIGN
C
      SUBROUTINE INFX(P2,NRGN,NCOMP,BBND, IVX,JVX,KVX,LVX)
C
CDEL  INTEGER RGX , MSX , ZNEX , ZDX , WZX
CDEL  PARAMETER ( RGX=100, MSX=211, ZDX=200, ZNEX=1000, WZX=100 )
      INCLUDE  'CITPMINC'
C
      REAL*8 P2
C
      COMMON/ALSUB/BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,
     & LMAX,MMAX, NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,
     & IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(100), IX(200),INNO(100),
     &  NGC(24),IEDG(24),ITMX(24),TIMX(6), GLIM(6),NDPL(24),IEDP1(24),
     & IEDP2(24),IEDP3(24), DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),
     & XSRH1(6), XTR1(WZX),XTR2(WZX),NXTR1(WZX),NXTR2(WZX),SPARE(200),
     & IXPUT(200),XPUT(200)
      COMMON/AFLUX/BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,
     & ISTART,IEP, VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,
     & XLEK,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3, NI3,IEXTR,
     & IRECV,VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2, BETTA,SUMXI,IX25,IX28,I,J,
     &  KB,K,ITMAX,ITIME, BET(MSX),DEL(MSX)
C
      DIMENSION P2(JVX,IVX,KVX),NRGN(JVX,IVX)
      DIMENSION NCOMP(LVX)
      DIMENSION BBND(KVX)
C
  100 IF (IX(24).EQ.0) REWIND IOFLX
      IF (IX(24).GT.0) GO TO 101
      IF (IX(39).NE.0) GO TO 101
      IF (IX(22).NE.0) GO TO 101
      IF ((NGC(2).NE.0).AND.(NUAC(2).LT.2)) GO TO 106
      IF (NGC(2).NE.0) GO TO 101
      IF (IX(16).GT.0) GO TO 106
      IF (IX(3).GT.1) GO TO 106
  101 CONTINUE
      XLPL = 1.0E-30
      IF(IX(24).GT.0.AND.PROD.LE.10.0) XLPL = 1.0
      DO 105 I = 1,IMAX
  102 DO 104 J = 1,JMAX
      DO 103 K = 1,KMAX
      T1 = 1.0E+12
      IF (IX(24).GT.0) T1 = P2 (J,I ,1)*XLPL
      P2 (J,I, K) = T1
  103 CONTINUE
  104 CONTINUE
  105 CONTINUE
  106 CONTINUE
      DO 109 K = 1,KMAX
      DO 108 I = 1,IMAX
      DO 107 J = 1,JMAX
      IF (P2(J,I,K).EQ.0.0) GO TO 107
      T2 = P2(J,I,K)
      GO TO 110
  107 CONTINUE
  108 CONTINUE
  109 CONTINUE
      WRITE(IOUT,1000)
      STOP
  110 CONTINUE
      DO 113 K = 1,KMAX
      DO 112 I = 1,IMAX
      DO 111 J = 1,JMAX
      IF (P2(J,I,K).EQ.0.0) P2 (J,I, K) = T2
  111 CONTINUE
  112 CONTINUE
  113 CONTINUE
      DO 120 I = 1,IMAX
      DO 119 J = 1,JMAX
      L = NRGN(J,I)
      M = NCOMP(L)
      DO 118 K = 1,KMAX
      IF (M-NUAC(17)) 117,114,117
  114 IF (XMIS(2)) 115,116,116
  115 IF (BBND(K)) 116,117,116
  116 P2(J,I,K) = 0.0
  117 CONTINUE
  118 CONTINUE
  119 CONTINUE
  120 CONTINUE
      IF (IX(39).NE.0) GO TO 121
      IF (IX(22).NE.0) GO TO 121
      IF ((NGC(2).GT.0).AND.(NUAC(2).EQ.0)) GO TO 131
  121 CONTINUE
      IGOTO = 2
      IF (IX(24).GT.0) IGOTO = 1
  122 BETTA=XMIS(6)
      VRGK2 = BETTA
  123 IF (IX(31)) 124,124,128
  124 IF (IMAX-1) 125,125,126
CNN
CN125 BETTA = 1.0
  125 IF(NUAC(5).GT.10) THEN
        BETTA = 1.8
      ELSE
        BETTA = 1.5
      ENDIF
CNN
      GO TO 128
  126 BETOI=3.141593/FLOAT(IMXP1)
      ARG1=COS(BETOI)
      BETOI=3.141593/FLOAT(JMXP1)
      ARG2=COS(BETOI)
      RHO=ARG1/(2.0-ARG2)
      T1 = KMAX
      T2 = (T1+11.0)/12.0
      RHO = RHO**T2
      BETTA = 2.0/(1.0+SQRT(1.0-RHO**2))
  127 RHO=ARG2/(2.0-ARG1)
      RHO = RHO**T2
      VRGK2 = 2.0/(1.0+SQRT(1.0-RHO**2))
  128 CONTINUE
  129 CONTINUE
  130 IF (NUAC(8).GE.0) GO TO 131
      BETTA=(6.0*BETTA+1.0)/7.0
      VRGK2=(6.0*VRGK2+1.0)/7.0
  131 RETURN
 1000 FORMAT('0ERROR STOP NUMBER 11'/' THE INITIAL FLUXES AN EIGENVA',
     &       'LUE CALCULATION ARE ALL ZERO.'                           )
      END