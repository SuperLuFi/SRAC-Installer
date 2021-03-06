CKNFX --094 ***CITATION*** INITIAL FLUX FOR 3-D/ CF- EIGN
C
      SUBROUTINE KNFX(P2E,NRGNE,NCOMP,BBND, IVX,JVX,KBVX,KVX,LVX,JIVX)
C
CDEL  INTEGER RGX , MSX , ZNEX , ZDX , WZX
CDEL  PARAMETER ( RGX=100, MSX=211, ZDX=200, ZNEX=1000, WZX=100 )
      INCLUDE  'CITPMINC'
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
      DIMENSION P2E(JIVX ,KBVX,KVX),NRGNE(JVX,IVX,KBVX)
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
      DO 105 KB=1,KBMAX
      DO 104 I=1,IMAX
      NN1= (I-1)*JVX
      DO 103 J=1,JMAX
      N1= NN1 + J
      DO 102 K=1,KMAX
      T1 = 1.0E+12
      IF (IX(24).GT.0) T1 = P2E(N1 ,KB,1)*XLPL
      P2E(N1 ,KB,K) = T1
  102 CONTINUE
  103 CONTINUE
  104 CONTINUE
  105 CONTINUE
  106 CONTINUE
      DO 110 K=1,KMAX
      DO 109 KB=1,KBMAX
      N1= 0
      DO 108 I=1,IMAX
      DO 107 J=1,JMAX
      N1= N1 + 1
      IF (P2E(N1,KB,K).EQ.0.0) GO TO 107
      T2 = P2E(N1,KB,K)
      GO TO 111
  107 CONTINUE
  108 CONTINUE
  109 CONTINUE
  110 CONTINUE
      WRITE(IOUT,1000)
      STOP
  111 CONTINUE
      DO 115 K = 1,KMAX
      DO 114 KB = 1,KBMAX
      N1= 0
      DO 113 I = 1,IMAX
      DO 112 J = 1,JMAX
      N1= N1 + 1
      IF (P2E(N1,KB,K).EQ.0.0) P2E(N1 ,KB,K) = T2
  112 CONTINUE
  113 CONTINUE
  114 CONTINUE
  115 CONTINUE
      DO 123 KB = 1,KBMAX
      DO 122 I = 1,IMAX
      NN1= (I-1)*JVX
      DO 121 J = 1,JMAX
      N1= NN1 + J
      L = NRGNE(J,I,KB)
      M = NCOMP(L)
      DO 120 K = 1,KMAX
      IF (M-NUAC(17)) 119,116,119
  116 IF (XMIS(2)) 117,118,118
  117 IF (BBND(K)) 118,119,118
  118 P2E(N1 ,KB,K) = 0.0
      GO TO 120
  119 CONTINUE
  120 CONTINUE
  121 CONTINUE
  122 CONTINUE
  123 CONTINUE
      IF (IX(39).NE.0) GO TO 124
      IF (IX(22).NE.0) GO TO 124
      IF ((NGC(2).GT.0).AND.(NUAC(2).EQ.0)) GO TO 130
  124 CONTINUE
      IGOTO = 2
      IF (IX(24).GT.0) IGOTO = 1
      BETTA = XMIS(6)
      VRGK2 = BETTA
      SPARE(39) = BETTA
  125 IF (IX(31)) 126,126,129
  126 IF (IMAX-1) 127,127,128
  127 BETTA = 1.0
      GO TO 129
  128 ARG1 = 3.141593/FLOAT(IMXP1)
      ARG2 = 3.141593/FLOAT(KBMXP1)
      ARG3 = 3.141593/FLOAT(JMXP1)
      XBET=(COS(ARG1)+COS(ARG2))/(3.0-COS(ARG3))
      YBET = AMAX1(XBET,0.95)
      T1=KMAX
      T2=(T1+11.0)/12.0
      ZBET=YBET**T2
      BETTA = 2.0/(1.0+SQRT(1.0-ZBET**2))
  129 CONTINUE
  130 RETURN
 1000 FORMAT('0ERROR STOP NUMBER 11'/
     &       ' THE INITIAL FLUXES FOR AN EIGENVALUE CALCULATION ARE AL',
     &       'L ZERO.' )
      END
