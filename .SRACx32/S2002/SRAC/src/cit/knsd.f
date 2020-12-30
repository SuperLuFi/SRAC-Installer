CKNSD --112 ***CITATION*** FLUX CALCULATION CONTROL FOR 3-D /CF-KLUX
C
      SUBROUTINE KNSD(SCATE,P2E,DCONBE,DCONRE,DCONBK,PTSAE,SOURE,NRGNE,
     & XII,SCAC,P1E,E1,IVX,JVX,KBVX,KVX,IVXP1,JVXP1,KBVXP1,LVX, JIVX,
     & JIP1VX,JP1IXZ,IOVX,IOVZ,SPAR,BIEMS,NCRP,NSPA, SIG,PVOL,NCOMP,MVX,
     &  AIO,IX3738,XLAMDA,XI,XL,B2,IOADJ,IOFS,KGP1)
C
CDEL  INTEGER RGX , MSX , ZNEX , ZDX , WZX
CDEL  PARAMETER ( RGX=100, MSX=211, ZDX=200, ZNEX=1000, WZX=100 )
      INCLUDE  'CITPMINC'
C
      REAL*8 XII
      REAL*8 XLAMDA
      REAL*8 SUMXI,TPTSA,XADB,XPDB,XS1DB,XS2DB, TL,XRDB,CS1S,CS2S,XLL1,
     & D8,XADX,YADX, XLL2,XLL3,XLL4,XLL5,XLL6,XLEK,B2LK,B3LK,B4LK,B5LK,
     & D1,D2,D3,D4,D5, D6,D7,YADB,YPDB,YLEK,YS1S,YS2S,YS1DB,YS2DB,YRDB,
     & SPR50,XLAST
C
      COMMON/ADUBP/SUMXI(ZNEX)
     &                  ,TPTSA,XADB,XPDB,XS1DB,XS2DB, TL,XRDB,CS1S,CS2S,
     &  XLL1,D8,XADX,YADX, XLL2,XLL3,XLL4,XLL5,XLL6,XLEK,B2LK,B3LK,B4LK,
     &  B5LK,D1,D2,D3,D4,D5, D6,D7,YADB,YPDB,YLEK,YS1S,YS2S,YS1DB,YS2DB,
     &  YRDB,SPR50,XLAST
      COMMON/ALSUB/BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,
     & LMAX,MMAX, NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,
     & IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(100), IX(200),INNO(100),
     &  NGC(24),IEDG(24),ITMX(24),TIMX(6), GLIM(6),NDPL(24),IEDP1(24),
     & IEDP2(24),IEDP3(24), DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),
     & XSRH1(6), XTR1(WZX),XTR2(WZX),NXTR1(WZX),NXTR2(WZX),SPARE(200),
     & IXPUT(200),XPUT(200)
      COMMON/AFLUX/BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,
     & ISTART,IEP, VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,
     & XELK,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3, NI3,IEXTR,
     & IRECV,VRGABS,LO3,LO4,XLAMDB,EPI1,EPI2, BETTA,SAMXI,IX25,IX28,I,J,
     &  KB,K,ITMAX,ITIME, BET(MSX),DEL(MSX)
C
      DIMENSION SCATE(JVX,IVX,KBVX), P2E(JIVX ,KBVX,KVX),DCONBE(JIP1VX ,
     &  KBVX,IOVX),DCONRE(JP1IXZ ,KBVX,IOVZ),DCONBK(JIVX , KBVXP1,IOVX),
     &  PTSAE(JIVX ,KBVX,IOVX),SOURE(JVX,IVX,KBVX), NRGNE(JVX,IVX,KBVX),
     &  XII(KVX,MVX),SCAC(KVX,MVX,KVX),P1E(JIVX,KBVX)
      DIMENSION E1(LVX,KVX)
      DIMENSION TSOUR(MSX)
      DIMENSION SPAR(NCRP,NSPA),BIEMS(KVX)
      DIMENSION SIG(KVX,MVX,10),PVOL(LVX),NCOMP(LVX)
      DIMENSION AIO(IX3738),XI(KVX,MVX),XL(6,KVX),B2(MVX,KVX)
C
CCCCC ********* SUBSCRIPT DEFINITIONS (KNSD E-061) ********* CCCCC
C    NEW         OLD            NEW         OLD
C     N1         J,I
C     N2         1,I             N5      JMAX,I
C     N3 *       1,I             N7         J,IMXP1
C     N4 *   JMXP1,I             N8         J,IMAX
C                                 J         J,1
C
C     INRB = 1  ORDINARY
C     INRB = 2  PERIODIC(REPEATING)
C     INRB = 3  90 DEGREE ROTATIONAL
C     INRB = 4  180 DEGREE ROTATIONAL
C
      INRB = IX(72) + 1
      IO19 = IX(86)
      IF (IX(135).EQ.1) REWIND IO19
      RMX = 1.0
      RMN = 1.0
      IX37 = IX(37)
      DO 152 KT1 = 1,KMAX
      IF (IX37.EQ.0) GO TO 102
      READ(IOADJ) AIO
      IF (IX(71).GT.0) GO TO 100
      K = KT1
      GO TO 101
  100 K = KGP1 - KT1
  101 CONTINUE
      IX(20) = 1
      GO TO 105
  102 CONTINUE
      IF (IX(24).GT.0) GO TO 103
      K = KT1
      GO TO 104
  103 K = KGP1-KT1
  104 CONTINUE
      IX(20) = K
  105 CONTINUE
      IF (IX(5).EQ.(-5)) GO TO 112
      IF (IX(24).EQ.0) GO TO 106
      IF (IX(17).GE.1) GO TO 108
  106 CONTINUE
      DO 107 L=1,LMAX
      M = NCOMP(L)
      E1(L,K) = XLAMDA*SIG(K,M,5)*PVOL(L)
      IF (IX(24).EQ.0) GO TO 107
      IF ((IX(17).EQ.-2).AND.(IX(71).GT.0)) E1(L,K) =SIG(K,M,5)*PVOL(L)
  107 CONTINUE
  108 CONTINUE
      IF (IX(24).GT.0) GO TO 111
C
C***************************SEARCH OPTIONS******************************
      IF ((IX(5).EQ.0).OR.(IX(5).GE.2)) GO TO 110
C 109 XII(K) = XI(K)/SPARE(50)
  109 CONTINUE
      DO 1109 M = 1,MMAX
 1109 XII(K,M) = XI(K,M)/SPARE(50)
      GO TO 111
C 110 XII(K) = XI(K)*XLAMDA
  110 CONTINUE
      DO 1110 M = 1,MMAX
 1110 XII(K,M) = XI(K,M)*XLAMDA
  111 CONTINUE
      GO TO 113
  112 CONTINUE
      IF (IX(132).GT.0) READ(IOFS) SPAR
C     BIEMS(K) = XLAMDA*XI(K)
      BIEMS(K) = XLAMDA*XI(K,1)
  113 CONTINUE
      NOINNR=NUAC(23)
      IF (IX(24).GT.0) GO TO 114
C
      KSCT1 = K-IX28
      IF (KSCT1.LE.0) KSCT1 = 1
      KSCT2 = MAX0((K-1),1)
      IF (K.GE.KXMN8) KSCT2 = KVX
C IRECV IS THE GROUP NO. WHICH CAN UPSCATTER TO GROUP 1. IT IS NOT BEING
C USED AND IS SET TO 0 IN KEGN.
      IF (K.LT.IRECV) KSCT2 = IRECV
      GO TO 115
  114 CONTINUE
      KSCT1 = K
      IF (K.GE.KXMN8) KSCT1 = KXMN8
      KSCT2 = K+IX28
      IF (KSCT2.GT.KVX) KSCT2 = KVX
      IF (K.LT.IRECV) KSCT1 = 1
  115 CONTINUE
      DO 122 KB=1,KBVX
      DO 121 I=1,IVX
      NN1 = (I-1)*JVX
      DO 120 J=1,JVX
      N1 = NN1 + J
      L = NRGNE(J,I,KB)
      M = NCOMP(L)
      P1E(N1,KB) = P2E(N1,KB,K)
      IF (IX(24).GT.0) GO TO 117
      CKSS = 0.0
      DO 116 KK=KSCT1,KSCT2
      CKSS = CKSS + SCAC(KK,M,K)*P2E(N1,KB,KK)
  116 CONTINUE
C     SCATE(J,I,KB) = CKSS*PVOL(L) + SOURE(J,I,KB)*XII(K)
      SCATE(J,I,KB) = CKSS*PVOL(L) + SOURE(J,I,KB)*XII(K,M)
      GO TO 119
  117 CKSS = SOURE(J,I,KB)*SIG(K,M,4)
      DO 118 KK= KSCT1,KSCT2
      CKSS = CKSS + SCAC(K,M,KK)*P2E(N1,KB,KK)
  118 CONTINUE
      SCATE(J,I,KB) = CKSS*PVOL(L)
  119 CONTINUE
  120 CONTINUE
  121 CONTINUE
  122 CONTINUE
      IF (IX(5).NE.(-5)) GO TO 126
      BM = BIEMS(K)
      NP = 0
      DO 125 KB=1,KBVX
      DO 124 I=1,IVX
      DO 123 J=1,JVX
      NP = NP + 1
      SCATE(J,I,KB) = SCATE(J,I,KB) + BM*SPAR(NP,1)
  123 CONTINUE
  124 CONTINUE
  125 CONTINUE
  126 CONTINUE
      IF (IX(135).EQ.1) WRITE(IO19) P1E
      DO 130 INNR=1,NOINNR
      IF (NUAC(5).EQ.14) GO TO 129
      IF (IX(72).EQ.1) GO TO 128
      IF (NUAC(5).EQ.13) GO TO 127
      CALL KWRD(SCATE,P2E,DCONBE,DCONRE,DCONBK,PTSAE,TSOUR, NRGNE,E1,
     & LVX, IVX,JVX,KBVX,KVX,IVXP1,JVXP1,KBVXP1, JIVX,JIP1VX,JP1IXZ,
     & IOVX,IOVZ)
      IF (IX(72).GT.1) GO TO 130
      IF (NUAC(20).GT.-1) GO TO 130
      CALL KXRD(SCATE,P2E,DCONBE,DCONRE,DCONBK,PTSAE,TSOUR, NRGNE,E1,
     & LVX, IVX,JVX,KBVX,KVX,IVXP1,JVXP1,KBVXP1, JIVX,JIP1VX,JP1IXZ,
     & IOVX,IOVZ)
      IF (NUAC(20).NE.-1) GO TO 130
      CALL KZRD(SCATE,P2E,DCONBE,DCONRE,DCONBK,PTSAE,TSOUR, NRGNE,E1,
     & LVX, IVX,JVX,KBVX,KVX,IVXP1,JVXP1,KBVXP1, JIVX,JIP1VX,JP1IXZ,
     & IOVX,IOVZ)
      GO TO 130
  127 CONTINUE
      CALL MWRD(SCATE,P2E,DCONBE,DCONRE,DCONBK,PTSAE,TSOUR, NRGNE,E1,
     & LVX, IVX,JVX,KBVX,KVX,IVXP1,JVXP1,KBVXP1, JIVX,JIP1VX,JP1IXZ,
     & IOVX,IOVZ)
      GO TO 130
  128 CALL KPER(SCATE,P2E,DCONBE,DCONRE,DCONBK,PTSAE,TSOUR, NRGNE, E1,
     & LVX, IVX,JVX,KBVX,KVX,IVXP1,JVXP1,KBVXP1, JIVX,JIP1VX,JP1IXZ,
     & IOVX,IOVZ)
      GO TO 130
  129 CONTINUE
      CALL KTRI(SCATE,P2E,DCONBE,DCONRE,DCONBK,PTSAE,TSOUR, NRGNE,E1,
     & LVX, IVX,JVX,KBVX,KVX,IVXP1,JVXP1,KBVXP1, JIVX,JIP1VX,JP1IXZ,
     & IOVX,IOVZ)
  130 CONTINUE
      IF ((NUAC(3).GT.0).OR.(IX(24).GT.0)) GO TO 147
      XLL1 = 0.0
      XLL2 = 0.0
      XLL3 = 0.0
      XLL4 = 0.0
      XLL5 = 0.0
      XLL6 = 0.0
      N = IX(20)
      NN3 = (IMXP1-1)*JVX
      NN4 = (IMAX-1)*JVX
      DO 131 M=1,MMAX
      B2(M,K) = 0.0
  131 CONTINUE
      DO 137 KB = 1,KBMAX
      DO 133 I = 1,IMAX
      N2= (I-1)*JVX
      N5= N2 + JMAX
      N2= N2 + 1
      N3= (I-1)*JVXP1
      N4=N3 + JMXP1
      N3= N3 + 1
      XLL1 = XLL1+P2E(N2 ,KB,K)*DCONRE(N3 ,KB,N)
      T1 = DCONRE(N4 ,KB,N)
      IF (T1-4096.0E-13) 132,133,132
  132 XLL3 = XLL3+P2E(N5 ,KB,K)*T1
  133 CONTINUE
      DO 136 J = 1,JMAX
      N7 = NN3 + J
      N8 = NN4 + J
      XLL2 = XLL2+P2E(J ,KB,K)*DCONBE(J ,KB,N)
      IF (NUAC(5).NE.14) GO TO 134
      IF (J.EQ.1) GO TO 136
      N7 = N7-1
  134 CONTINUE
      T1 = DCONBE(N7 ,KB,N)
      IF (T1-4096.0E-13) 135,136,135
  135 XLL4 = XLL4+P2E(N8 ,KB,K)*T1
  136 CONTINUE
  137 CONTINUE
      N1= 0
      DO 140 I=1,IMAX
      DO 139 J=1,JMAX
      N1= N1 + 1
      XLL5 = XLL5+P2E(N1 ,1,K)*DCONBK(N1 ,1,N)
      T1 = DCONBK(N1 ,KBMXP1,N)
      IF (T1-4096.0E-13) 138,139,138
  138 XLL6 = XLL6+P2E(N1 ,KBMAX,K)*T1
  139 CONTINUE
  140 CONTINUE
      GO TO (141,142,143,144),INRB
  141 XLEK = XLEK + XLL1 + XLL2 + XLL3 + XLL4 + XLL5 + XLL6
      GO TO 145
  142 XLEK = XLEK + XLL2 + XLL4 + XLL5 + XLL6
      XLL1 = XLL1 - XLL3
      XLL3 = (-XLL1)
      GO TO 145
  143 XLEK = XLEK + XLL1 + XLL2 + XLL5 + XLL6
      XLL4 = (-XLL3)
      GO TO 145
  144 XLEK = XLEK + XLL1 + XLL2 + XLL4 + XLL5 + XLL6
      XLL3 = 0.0
  145 CONTINUE
      XL(1,K) = XLL1
      XL(2,K) = XLL2
      XL(3,K) = XLL3
      XL(4,K) = XLL4
      XL(5,K) = XLL5
      XL(6,K) = XLL6
      IF (NUAC(17).LE.0) GO TO 146
      CALL KINS(P2E,B2,NRGNE,DCONBE,DCONRE,DCONBK,SCAC,XL, IVX, JVX,
     & KBVX,KVX,LVX,JVXP1,KBVXP1,JIVX,JIP1VX,JP1IXZ,IOVX,IOVZ, PVOL,
     & NCOMP,MVX,IVXP1)
  146 CONTINUE
  147 CONTINUE
      DO 151 KB=1,KBMAX
      N1 = 0
      DO 150 I=1,IMAX
      DO 149 J=1,JMAX
      N1 = N1 + 1
      TT1 = P1E(N1,KB)
      T2 = P2E(N1,KB,K)
      IF (TT1.EQ.0.0) GO TO 148
      RATO = T2/TT1
      RMX = AMAX1(RMX,RATO)
      RMN = AMIN1(RMN,RATO)
  148 CONTINUE
  149 CONTINUE
  150 CONTINUE
  151 CONTINUE
  152 CONTINUE
      IF (IX(135).EQ.1) REWIND IO19
      RETURN
      END