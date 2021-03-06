CKZRD --123***CITATION*** LINE RELAXATION BKWD 3-D/ CF-KNSD
C
      SUBROUTINE KZRD(SCATE,P2E,DCONBE,DCONRE,DCONBK,PTSAE,TSOUR, NRGNE,
     &  E1,LVX, IVX,JVX,KBVX,KVX,IVXP1,JVXP1,KBVXP1, JIVX,JIP1VX,JP1IXZ,
     &  IOVX,IOVZ)
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
      DIMENSION SCATE(JVX,IVX,KBVX),P2E(JIVX,KBVX,KVX), DCONBE(JIP1VX,
     & KBVX,IOVX),DCONRE(JP1IXZ,KBVX,IOVZ), DCONBK(JIVX,KBVXP1,IOVX),
     & PTSAE(JIVX,KBVX,IOVX)
      DIMENSION E1(LVX,KVX),NRGNE(JVX,IVX,KBVX)
      DIMENSION TSOUR(MSX)
C
CCCCC ********* SUBSCRIPT DEFINITIONS (KZRD E-085) ********* CCCCC
C    NEW         OLD            NEW         OLD
C     N1         J,I             N7         L,I
C     N2       J-1,I             N8        KB,I
C     N3       J+1,I             N9  *      J,I
C     N4         1,I             N10 *    J+1,I
C     N5         2,I             N11 *      2,I
C     N6       JVX,I             N12 *     JVX,I
C                                N13        J,I-1
C                                N14        J,I+1
C
      N = IX(20)
      DO 142 I=1,IVX
      NN1= (I-1)*JVX
      N4= NN1 + 1
      N5= NN1 + 2
      N6= NN1 + JVX
      NN2= (I-1)*JVXP1
      N11= NN2 + 2
      N12= NN2 + JVX
  100 DO 141J=1,JVX
      N1= NN1 + J
      N13 = N1 - JVX
      N14 = N1 + JVX
      DO 103 KB=1,KBVX
      CKSS = SCATE(J,I,KB)
      IF (I.LE.1) GO TO 101
      CKSS = CKSS + P2E(N13,KB,K)*DCONBE(N1,KB,N)
  101 IF (I.GE.IVX) GO TO 102
      CKSS = CKSS + P2E(N14,KB,K)*DCONBE(N14,KB,N)
  102 TSOUR(KB) = CKSS
  103 CONTINUE
      JP1=J-1
      JP2=J+1
      N2= NN1 + JP1
      N3= NN1 + JP2
      N9= NN2 + J
      N10= NN2 + JP2
      DEL(1)=0.0
      D1=DCONRE(N9 ,1,N)
      D2=DCONRE(N10 ,1,N)
      D4=DCONBK(N1 ,2,N)
      IF (J-1) 104,104,110
  104 IF (P2E(N4,1,K)) 105,106,105
  105 BET(1)=(P2E(N5 ,1,K) *D2+TSOUR(1) )/D4
      L = NRGNE(1,I,1)
      DEL(1) = D4/(PTSAE(N4,1,N) + E1(L,K))
  106 DO 109KB=2,KBVX
      IF (P2E(N4,KB,K)) 108,107,108
  107 DEL(KB)=0.0
      GO TO 109
  108 T=D4*DEL(KB-1)
      L = NRGNE(1,I,KB)
      D4=DCONBK(N4 ,KB+1,N)
      BET(KB)=(P2E(N5 ,KB,K)*DCONRE(N11,KB,N)+TSOUR(KB) +BET(KB-1)*T)/D4
      DEL(KB) = D4/(PTSAE(N4,KB,N) - T + E1(L,K))
  109 CONTINUE
      GO TO 123
  110 IF (J-JVX) 117,111,111
  111 IF (P2E(N6,1,K)) 112,113,112
  112 BET(1)=(P2E(N2 ,1,K)*D1+TSOUR(1) )/D4
      L = NRGNE(JVX,I,1)
      DEL(1) = D4/(PTSAE(N6,1,N) + E1(L,K))
  113 DO 116KB=2,KBVX
      IF (P2E(N6,KB,K)) 115,114,115
  114 DEL(KB)=0
      GO TO 116
  115 T=D4*DEL(KB-1)
      L = NRGNE(JVX,I,KB)
      D4=DCONBK(N1 ,KB+1,N)
      BET(KB)=(P2E(N2 ,KB,K)*DCONRE(N12 ,KB,N)+TSOUR(KB) +BET(KB-1)*T)/
     & D4
      DEL(KB) = D4/(PTSAE(N6,KB,N) - T + E1(L,K))
  116 CONTINUE
      GO TO 123
  117 IF (P2E(N1,1,K)) 118,119,118
  118 CONTINUE
      L = NRGNE(J,I,1)
      BET(1)=(P2E(N2 ,1,K)*D1+P2E(N3 ,1,K)*D2+TSOUR(1) )/D4
      DEL(1) = D4/(PTSAE(N1,1,N) + E1(L,K))
  119 DO 122KB=2,KBVX
      IF (P2E(N1,KB,K)) 121,120,121
  120 DEL(KB)=0
      GO TO 122
  121 T=D4*DEL(KB-1)
      L = NRGNE(J,I,KB)
      D4=DCONBK(N1 ,KB+1,N)
      BET(KB)=(P2E(N2 ,KB,K)*DCONRE(N9 ,KB,N)+P2E(N3 ,KB,K)*DCONRE( N10
     & ,KB,N)+TSOUR(KB) +BET(KB-1)*T)/D4
      DEL(KB) = D4/(PTSAE(N1,KB,N) - T + E1(L,K))
  122 CONTINUE
  123 TEMP=BET(KBVX)*DEL(KBVX)
      T=P2E(N1 ,KBVX,K)
      TMF=T+SPARE(39)*(TEMP-T)
      IF (IEP) 124,128,125
  124 P2E(N1 ,KBVX,K)=TEMP
      GO TO 129
  125 IF (TMF-TEMP) 127,128,126
  126 TMF=AMIN1(TMF,(TEMP+T))
      GO TO 128
  127 TMF=AMAX1(TMF,0.5*TEMP)
  128 CONTINUE
      P2E(N1 ,KBVX,K)=TMF
  129 DO 136KK=2,KBVX
      KB=KBVXP1-KK
      T=P2E(N1 ,KB,K)
      TEMP=DEL(KB)*(TEMP+BET(KB))
      TMF=T+SPARE(39)*(TEMP-T)
      IF (IEP) 130,134,131
  130 P2E(N1 ,KB,K)=TEMP
      GO TO 136
  131 IF (TMF-TEMP) 133,134,132
  132 TMF=AMIN1(TMF,(TEMP+T))
      GO TO 134
  133 TMF=AMAX1(TMF,0.5*TEMP)
  134 CONTINUE
  135 P2E(N1 ,KB,K)=TMF
  136 CONTINUE
      IF (NUAC(9)) 137,141,139
  137 L=JVXP1-J
      N7= NN1 + L
      DO 138KB=1,KBVX
      M=KBVXP1-KB
      P2E(N7 ,M,K)=P2E(N1 ,KB,K)
  138 CONTINUE
      GO TO 141
  139 DO 140KB=1,KBVX
      N8= NN1 + KB
      P2E(N8 ,J,K)=P2E(N1 ,KB,K)
  140 CONTINUE
  141 CONTINUE
  142 CONTINUE
      RETURN
      END
