CFLUX --095 ***CITATION*** EIGENVALUE-FLUX FOR 1,2-D/ CF-EIGN
C
      SUBROUTINE FLUX(NRGN,E1, B1,B2,B3,B4,B5,SCAC, CONC, P1,P2,DCONB,
     & DCONR,SOUR,SCAT,PTSA,NCOMP, F1,SS1,SS2,SS3,SS4, SS5,SSC,SIG,HOL,
     & ONEOV,PVOL,NJJR, BBND,BND,XI,XL,XII, IVX,JVX, KVX, LVX,MVX,NVX,
     & IVXP1,JVXP1,IVZ,KVZ,NSETVX, IOVX,IOVZ,A,MEMORY, AIO,IX3738,SPAR,
     & BIEMS,NCRP,NSPA,ZONEN,NNXTRA,NVO)
C
CDEL  INTEGER RGX , MSX , ZNEX , ZDX , WZX
CDEL  PARAMETER ( RGX=100, MSX=211, ZDX=200, ZNEX=1000, WZX=100 )
      INCLUDE  'CITPMINC'
C
      REAL*8 P2,SCAT,SOUR
      REAL*8 B1
      REAL*8 PNM,PNM0,PNM1,PNM2
      REAL*8 BET(MSX),DEL(MSX)
      REAL*8 XII
      REAL*8 XLAMDA
      REAL*8 XAAMDA
      REAL*8 TXX1
      REAL*8 ZPDB,XMU3
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
     &  KB,K,ITMAX,ITIME, BAT(MSX),DAL(MSX)
      COMMON/AKADD/KAY(1),K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11, K12,K13,
     & K131,K14,K15,K16,K17,K18,K19,K20,K21,K22,K23,K24, K25,K26,K27,
     & K28,K29,K30,K31,K32,K33,K34,K35,K36,K37, K38,K39,K40,K41,K42,K43,
     &  K44,K45,K46,K47,K48,K49, K50,K51,K52,K53,K54,K55,K56,K57,K58,
     & K59, K60,K61,K62, K63,K64,K65,K66,K67,K68,K69,K70,K71,K72,K73,
     & K74,K75, K76,K77,K78,K79,K80,K81,K82,K83,K84,K85,K86,K87, K88,
     & K89,K90, K91,K92,K93,K94,K95,K96,K97,K98, K99,K100,NDATA,KNRGN,
     & KNCOMP, KPVOL,KRVOL,MEMVRY, MEMX
C
      COMMON /CRBNC/ ICONV,ICRBN,NCBSTP,AVZAB(ZNEX)
C     ICRBN IS SET IN CIT1 IF SRAC, OR CRBN IF COREBN
C     ICRBN =0 : SRAC-CITATION, ICRBN=1 : COREBN
C
      DIMENSION E1(LVX,KVX), B1(MVX,KVX), B2(MVX, KVX) ,B3(MVX,KVX),
     & SCAC(KVX,MVX,KVX), CONC(NVX,MVX),B4(MVX,KVX), B5(MVX, KVX),
     & P1(JVX,IVX) ,P2(JVX,IVX,KVX), DCONB(JVX,IVXP1, IOVX),
     & DCONR(JVXP1,IVZ,IOVZ),NRGN(JVX,IVX), SOUR(JVX,IVX), SCAT(JVX,IVX)
     & , PTSA(JVX,IVX,IOVX),NCOMP(LVX)
      DIMENSION F1(KVX ,MVX),SS1(KVX,NVX,NSETVX), SS2(KVX,NVX,NSETVX) ,
     & SS3(KVX,NVX,NSETVX),SS4(KVX,NVX,NSETVX), SS5(KVX,NVX,NSETVX),
     & SSC(KVX,KVX,NVX),HOL(NVX,NSETVX,10), ONEOV(KVX,NSETVX),PVOL(LVX),
     &  NJJR(NVX,NSETVX),SIG(KVX,MVX,10)
      DIMENSION BBND(KVX),BND(6,KVX),XI(KVX,MVX),XL(6,KVX),XII(KVX,MVX)
      DIMENSION AIO(IX3738)
      DIMENSION A(MEMORY)
      DIMENSION SPAR(NCRP,NSPA),BIEMS(KVX)
      DIMENSION ZONEN(NVO),NNXTRA(NVX,NSETVX)
C
      IF (ICRBN.EQ.1) GOTO 100
      ISTART = ICLOCK(0)
  100 XLAMDA = XLAMDB
      IX(129) = 0
      SPARE(29) = 1.0
      IF (IX(128).GT.0) SPARE(29) = 0.0
      SPR50 = 1.0/SPARE(50)
C     SUMXI = 0.0
C     DO 101 K = 1,KMAX
C     SUMXI = SUMXI+XI(K)
C 101 CONTINUE
      DO 101 M=1,MMAX
      SUMXI(M) = 0.0
      DO 101 K=1,KMAX
      SUMXI(M) = SUMXI(M)+XI(K,M)
  101 CONTINUE
      IX37 = IX(37)
      IX38 = IX(38)
      IO19 = IX(86)
      IO15 = IX(82)
      IOADJ = IO15
      IF (IX(71).GT.0) IOADJ = IO2
      IF (IX37.GT.0) REWIND IOADJ
      CALL BEGN(XL,P2,E1,XLAMDA,IVX,JVX,KVX,LVX)
      XPDB = 1.0
      YPDB = 1.0
      BETTX = BETTA
      IF (IX(198).NE.0) GO TO 102
      IF ((NGC(2).NE.0).AND.(NUAC(2).EQ.0)) GO TO 103
  102 CONTINUE
      BETTX = 1.0
      IEP = -1
  103 CONTINUE
CKSK  L211 = 211
      L211 = MSX
      DO 104 N = 1,L211
      BET(N) = 0.0
      DEL(N) = 0.0
  104 CONTINUE
      IF (IX(5).NE.(-5)) GO TO 106
      DO 105 M=1,MMAX
      DO 105 K=1,KMAX
      XII(K,M) = XI(K,M)
      XI(K,M) = BIEMS(K)
  105 CONTINUE
      IF (IX(132).LE.0) GO TO 106
      IOFS = IX(84)
      REWIND IOFS
  106 CONTINUE
      IRR = IX(10)
      JRR = IX(11)
      KRR = IX(13)
      IXM1=IVX-1
      JXM1=JVX-1
      NUPDTE = 0
      IEND = 0
      KGP1 = KMAX+1
      NOINNR = NUAC(23)
      NUAC20 = NUAC(20)
      PNM0 = 0.0
      PNM1 = 0.0
      CALL LOOP( P2,SOUR,NRGN,XII,IVX,JVX,KVX,LVX,XLAMDA, SIG,PVOL,
     & NCOMP,MVX)
  107 CONTINUE
      PNM2 = PNM1
      PNM1 = PNM0
      YADX = XADX
      ZPDB = YPDB
      YPDB = XPDB
      YLEK = XLEK
      YS1S = CS1S
      YS2S = CS2S
      YS1DB = XS1DB
      YS2DB = XS2DB
      XMU3 = 0.0
      XADB = 0.0
      XPDB = 0.0
      XLEK = 0.0
      XRDB = 0.0
      XS1DB = 0.0
      XS2DB = 0.0
      IF (NUAC(5).EQ.10) GO TO 108
      IF (IVX.NE.1) GO TO 108
      IF (IX(72).GT.0) GO TO 108
      CALL WFLX(P1,P2,SOUR,SCAT,SCAC,DCONR,PTSA,NRGN,E1, BET,DEL,XII,
     & IVX,JVX,KVX,LVX,IVXP1,JVXP1,IVZ,KVZ,IOVX,IOVZ, SPAR,BIEMS,NCRP,
     & NSPA,SIG,PVOL,NCOMP,MVX, AIO,IX3738,XLAMDA,XI,XL,B2,IOADJ,IOFS,
     & KGP1)
      GO TO 109
  108 CONTINUE
      CALL DNSD(P1,P2,SOUR,NRGN,SCAC,SCAT,XII,DCONB,DCONR, PTSA,BET,DEL,
     & E1,IVX,JVX,KVX,LVX,IVXP1,JVXP1,IVZ,KVZ,IOVX,IOVZ, SPAR,BIEMS,
     & NCRP,NSPA,SIG,PVOL,NCOMP,MVX, AIO,IX3738,XLAMDA,XI,XL,B2,IOADJ,
     & IOFS,KGP1)
  109 CONTINUE
      IF ((NUAC(3).GT.0).OR.(IX(24).GT.0)) GO TO 113
      CALL ABPR(P2,B1,B2,B3,B4,B5,NRGN,IVX,JVX,KVX,LVX, SIG,PVOL,NCOMP,
     & MVX)
      XELK = XLEK
      XKEF2 = XKEF1
      SPARE(43) = XLAMDA
C     XPDB = XPDB*SUMXI
      IF (YPDB.NE.ZPDB) XMU3 = (XPDB-YPDB)/(YPDB-ZPDB)
      XADX = XADB+XRDB
C***************************SEARCH OPTIONS*****************************
      IF ((IX(5).EQ.0).OR.(IX(5).GE.2)) GO TO 110
      CALL GINS(B1,B3,B4,B5,KVX,LVX,XLAMDA,MVX)
      IF (IX(5).EQ.(-5)) GO TO 112
      GO TO 111
  110 CONTINUE
      XLAMDA = (XLEK+XADX)/XPDB
      XKEF1 = 1.0/XLAMDA
      VRGK1 = ABS(XKEF2/XKEF1-1.0)
  111 CONTINUE
      XABS = XADX+XLAMDA*XS1DB
      PROD = XPDB+XLAMDA*XS2DB
      SPARE(48) = XS1DB
      SPARE(49) = XS2DB
      GO TO 113
  112 CONTINUE
      PROD = XPDB
      XABS = XADX
      IF (IX(132).GT.0) REWIND IOFS
  113 CONTINUE
      IF (IX37.GT.0) REWIND IOADJ
      IF (BETTA-1.0) 114,116,114
  114 IF (IEP) 115,116,116
  115 IEP=1
  116 CONTINUE
      IX(134) = 0
      IF ((NUAC(3).GT.0).OR.(IX(24).GT.0)) GO TO 120
      XABT = ABS(XABS)
CUNIX IF ((XABT.LT.1.0E+40).AND.(XABT.GT.1.0E-10)) GO TO 120
      IF ((XABT.LT.1.0E+35).AND.(XABT.GT.1.0E-10)) GO TO 120
      UP = 1.0E-30
      IF (XABT.LT.1.0E-10) UP = 1.0E+30
      IF (NI3.LE.3) GO TO 120
      IF (VRGP2.EQ.0.0) GO TO 120
      IF ((VRGP1/VRGP2).GE.1.0) GO TO 120
      DO 119 K = 1,KMAX
      DO 118 I = 1,IMAX
      DO 117 J = 1,JMAX
      P2(J,I,K) = P2(J,I,K)*UP
  117 CONTINUE
  118 CONTINUE
  119 CONTINUE
      PNM1 = PNM1*UP
      PNM2 = PNM2*UP
      IX(134) = 1
      IF (IX(5).NE.(-5)) GO TO 120
      XLAMDA = XLAMDA*UP
  120 CONTINUE
      P2RR = P2(JRR,IRR, KRR)
CUNIX IF ((P2RR.GT.0.0).AND.(P2RR.LT.1.0E+60)) GO TO 121
      IF ((P2RR.GT.0.0).AND.(P2RR.LT.1.0E+35)) GO TO 121
      WRITE(IOUT,1003)JRR,IRR, KRR,P2RR
      WRITE(IOUT,2000)
      IF((P2RR.LE.0.0).AND.(NUAC(18).GT.0)) GO TO 121
      STOP
  121 CONTINUE
      NUPDTE = NUPDTE+1
      IF (NSRH(11).EQ.0) NUPDTE = -1
      CALL LOOP( P2,SOUR,NRGN,XII,IVX,JVX,KVX,LVX,XLAMDA, SIG,PVOL,
     & NCOMP,MVX)
      IF (RMN.GE.0.0) GO TO 122
      IF(NUAC(18).GT.0) GO TO 122
      WRITE(IOUT,1003)
      WRITE(IOUT,2000)
      STOP
  122 CONTINUE
      PNM0 = P2(JRR,IRR,KRR)
      P2DOM = 2.0*PNM1-PNM2-PNM0
      IF (P2DOM.NE.0.0) PNM = (PNM0-PNM1)/P2DOM
      SPARE(35) = PNM
      IF (IX(5).GE.2.AND.IX(73).EQ.2) GO TO 135
      CALL EXTR
      IX(129) = 0
      XT1 = SPARE(31)
      XT2 = SPARE(32)
      XT3 = SPARE(33)
      XT4 = SPARE(34)
      IF (IX(32)) 135,135,123
  123 CONTINUE
      IEP = -1
      REWIND IO19
      DO 128 KT1=1,KMAX
      IF (IX(24).GT.0) GO TO 124
      K = KT1
      GO TO 125
  124 K = KGP1 - KT1
  125 CONTINUE
      READ(IO19) P1
      DO 127 I = 1,IMAX
      DO 126 J = 1,JMAX
      T1 = P1(J,I)
      IF (IX(134).EQ.1) T1 = T1*UP
      P2(J,I,K) = P2(J,I,K)+(P2(J,I,K)- T1)*EXFC1
  126 CONTINUE
  127 CONTINUE
  128 CONTINUE
      REWIND IO19
      IF ((NUAC(3).GT.0).OR.(IX(24).GT.0)) GO TO 134
      D1 = XPDB+(XPDB-YPDB)*EXFC1
      D2 = XADX+(XADX-YADX)*EXFC1
      D3 = XLEK+(XLEK-YLEK)*EXFC1
C***************************SEARCH OPTIONS*****************************
      IF ((IX(5).EQ.0).OR.(IX(5).GE.2)) GO TO 131
      IF (IX(5).EQ.(-5)) GO TO 132
      D6 = XS1DB+(XS1DB-YS1DB)*EXFC1
      D7 = XS2DB+(XS2DB-YS2DB)*EXFC1
      D8 = XLAMDA
      TL = D2+D3+D8*D6
      XKEF1 = (D1+D8*D7)/TL
      XAAMDA = (SPR50*D1-D2-D3)/( D6 -SPR50*D7)
      TXX1 = SPARE(51)
      IF (SPARE(51).GT.0.0) GO TO 129
      XLAMDA = DMAX1(XAAMDA,TXX1)
      GO TO 130
  129 CONTINUE
      XLAMDA = DMIN1(XAAMDA,TXX1)
  130 CONTINUE
      GO TO 134
  131 CONTINUE
      XLAMDA = (D3+D2)/D1
      XKEF1 = 1.0/XLAMDA
      GO TO 134
  132 CONTINUE
      D8 = (D3 + D2 - D1)/SPARE(88)
      IF (D8.GE.0.0) GO TO 133
      IF (D8.LT.XLAMDA) GO TO 134
  133 XLAMDA = D8
  134 CONTINUE
      CALL LOOP( P2,SOUR,NRGN,XII,IVX,JVX,KVX,LVX,XLAMDA, SIG,PVOL,
     & NCOMP,MVX)
  135 CONTINUE
      IF (NUAC(3)) 137,137,136
  136 CONTINUE
      CALL RDUE(SCAC, RESLM,RESSA, P2 ,NRGN , SOUR ,DCONR ,DCONB ,XI,
     & IVX,JVX,KVX,LVX,IVXP1,JVXP1,IVZ,KVZ, IOVX,IOVZ,A,MEMORY,AIO,
     & IX3738,XLAMDA, SIG,PVOL,NCOMP,MVX)
  137 CONTINUE
      INOW = ICLOCK(0)
      XWACH = (FLOAT(INOW)-FLOAT(ISTART))/6000.0
      SPARE(66) = XWACH
      T1 = ABS(2.0*VRG1)
      T2 = ABS(VRGABS)
      IF (T1-T2) 139,139,138
  138 VRGABS = T1
  139 CONTINUE
      CALL RQED(IX(101),IND)
      IF (IND.NE.0) GO TO 140
      CALL ITED(XLAMDA,XMU3,BETTX,XT1,XT2,XT3)
  140 CONTINUE
      IF (IX(5).GE.2.AND.IX(73).EQ.2) GO TO 169
  141 IWACH = (INOW-ISTART)/6000
      IS1 = 0
      IF (IX(6)) 143,143,142
  142 NR = 16
      GO TO 161
  143 CONTINUE
      IF (IEND.GT.0) GO TO 151
      IF (IX(5).NE.1) GO TO 145
      UPSIG = 0
      IF (NUPDTE.LT.NSRH(11)) GO TO 145
      IF (NIIT.GT.0) GO TO 145
      NGOTO = 1
      NUPDTE = 0
  144 CONTINUE
      CALL UDTE(CONC,SS1,SS2,SS3,SS4,SS5,SSC,SIG,HOL,ONEOV, SCAC,NJJR,
     & BND,BBND,ZONEN,NNXTRA,XI,XLAMDA,A(K19), MVX,NVX,KVX,NSETVX,NVO)
      CALL CNST(NRGN, SCAC,DCONB, DCONR,F1,SIG, PTSA,NCOMP,PVOL,BBND,
     & BND, IVX,JVX,KVX,LVX,MVX,IVXP1,JVXP1,IVZ, KVZ, IOVX,IOVZ,A,
     & MEMORY,AIO,IX3738)
      IX(129) = 1
      UPSIG = 1
      GO TO (145,150),NGOTO
  145 CONTINUE
      BETTX = BETTA
      IF (IEP.LT.0) BETTX = 1.0
      IF (NI3.LT.3) GO TO 146
      IF (VRGP1.GE.EPI1) GO TO 146
      IF (VRGK1.GE.EPI2) GO TO 146
      GO TO 148
C 146 IF (IWACH.GE.ITIME) GO TO 147
  146 IF (IWACH.GE.ITIME) GO TO 1147
      IF (NIT.GE.ITMAX) GO TO 147
      GO TO 107
 1147 ICONV = 1
  147 IS1 = 1
  148 CONTINUE
  149 CONTINUE
C**************************SEARCH OPTION********************************
      IF (IX(5).NE.1) GO TO 152
      IF (UPSIG.EQ.1) GO TO 150
      NGOTO = 2
      GO TO 144
  150 IF (NSRH(11).EQ.0) GO TO 151
      IEND = 1
      GO TO 107
  151 CONTINUE
  152 CONTINUE
      IF (IX(24).EQ.0) SPARE(56) = XLEK+XABS
      CALL RQED(IX(101),IND)
      IF (IND.EQ.0) GO TO 153
      CALL ITED(XLAMDA,XMU3,BETTX,XT1,XT2,XT3)
  153 CONTINUE
C
C***************************SEARCH OPTIONS******************************
      IF ((IX(5).EQ.0).OR.(IX(5).GE.2)) GO TO 154
      IF (IX(5).EQ.(-5)) GO TO 156
      GO TO 157
  154 CONTINUE
      IF (IX(24).EQ.0) GO TO 155
      WRITE(IOUT,1004) XWACH
      GO TO 157
  155 CONTINUE
      WRITE(IOUT,1005) XWACH
      GO TO 157
  156 WRITE(IOUT,1000) XWACH
  157 CONTINUE
      IF (IX(5).NE.1) GO TO 158
C*****CALL EDSR(CONC,NJJR,MVX,NVX,NSETVX)
  158 CONTINUE
      IF (IS1) 163,163,159
  159 CONTINUE
      IF (NGC(15).EQ.0) GO TO 162
      IF (NGC(15).EQ.2) GO TO 160
      IF (VRGP2.EQ.0.0) GO TO 162
      IF ((VRGP1/VRGP2).GE.1.0) GO TO 160
      GO TO 162
  160 NR = 13
  161 WRITE(IOUT,1006)NR
      IF (NR.EQ.13) WRITE(IOUT,2001)
      IF (NR.EQ.16) WRITE(IOUT,2002)
      STOP
  162 CONTINUE
      WRITE(IOUT,1001)
  163 CONTINUE
      IF (NUAC(3).EQ.0) GO TO 164
      WRITE(IOUT,1002)
      STOP
  164 IF (NGC(18)) 166,165,165
  165 CONTINUE
      IF (IX(5).EQ.(-5)) GO TO 166
      CALL RDUE(SCAC, RESLM,RESSA, P2 ,NRGN , SOUR ,DCONR ,DCONB ,XI,
     & IVX,JVX,KVX,LVX,IVXP1,JVXP1,IVZ,KVZ, IOVX,IOVZ,A,MEMORY,AIO,
     & IX3738,XLAMDA, SIG,PVOL,NCOMP,MVX)
      WRITE(IOUT,1008)RESSA,RESLM
  166 CONTINUE
      XLAMDB = XLAMDA
      IF (IX(5).NE.(-5)) GO TO 168
C     DO 167 K=1,KMAX
C     BIEMS(K) = XI(K)
C     XI(K) = XII(K)
C 167 CONTINUE
      DO 167 M=1,MMAX
      DO 167 K=1,KMAX
      BIEMS(K) = XI(K,1)
      XI(K,M)  =XII(K,M)
  167 CONTINUE
  168 CONTINUE
  169 CONTINUE
      RETURN
 1000 FORMAT(1H0,'END OF FIXED SOURCE CALCULATION - ITERATION TIME',
     & 0PF7.3,' MINUTES')
 1001 FORMAT(1H0/'0**********WARNING - FLUX CALCULATION NOT CONVERGED*',
     &'*********'/1H0/1H0)
 1002 FORMAT('0FLUX CALCULATION WAS DONE WITH THE RESIDUES - CALLING E',
     &'XIT NOW')
 1003 FORMAT(1H0,'ERROR STOP NUMBER 12',3I4,1PE13.5)
 1004 FORMAT(1H0,'END OF ADJOINT CALCULATION - ITERATION TIME',0PF7.3,
     & ' MINUTES')
 1005 FORMAT('0END OF EIGENVALUE CALCULATION - ITERATION TIME',0PF7.3,
     & ' MINUTES')
 1006 FORMAT(1H0,'ERROR STOP',I3)
 1007 FORMAT(1H0,73X,'NEUTRON BALANCE KEFFECTIVE',0PF14.7)
 1008 FORMAT('0CONVERGENCE INDICATION BY MINIMIZING THE SUM OF THE SQU',
     &'ARES OF THE RESIDUES - RELATIVE ABSORPTION',F11.7,'   K',F11.7)
 2000 FORMAT(1H ,'A NEGATIVE FLUX WAS CALCULATED, OR A FLUX WAS CALCUL',
     &       'ATED GREATER THAN 1.0E+35')
CUNIX&       'ATED GREATER THAN 1.0E+60')
 2001 FORMAT(1H ,'THE LIMIT ON MULTIPLICATION FACTOR, MACHINE TIME, OR',
     &       ' THE NUMBER OF ITERATIONS WAS EXCEEDED IN SOME LOOP CALCU'
     &      ,'LATION '/' AND THE OPTION TO TERMINATE (NGC15, SECTION ',
     &       '001) IS EXERCISED.'                                      )
 2002 FORMAT(1H ,'NO INITIAL VALUES OF THE SEARCH PARAMETERS (1/V,B**2',
     &       ',OR CONCENTRATIONS) WERE INPUT FOR A DIRECT SEARCH.'   )
      END
