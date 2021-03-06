CDASH --104 ***CITATION*** FLUX DRIVING FOR INDIRECT       / CF-FLUX
C                          SEARCH 1,2D
C
      SUBROUTINE DASH(P2,SOUR, JVX,IVX,KVX)
C
CDEL  INTEGER RGX , MSX , ZNEX , ZDX , WZX
CDEL  PARAMETER ( RGX=100, MSX=211, ZDX=200, ZNEX=1000, WZX=100 )
      INCLUDE  'CITPMINC'
C
      REAL*8 P2,SOUR
      REAL*8 ALPHA,X,PN,PNM1,PNP1,PRD
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
      DIMENSION P2(JVX,IVX,KVX),SOUR(JVX,IVX)
C
      IF (IX(73).GT.1) GO TO 100
      NFLXA = IOFLX
      NFLXB = IX(84)
      GO TO 101
  100 CONTINUE
      NFLXA = IX(152)
      NFLXB = IX(153)
      ITEMP = NFLXB
      NFLXB = NFLXA
      NFLXA = ITEMP
  101 CONTINUE
      REWIND NFLXA
      WRITE(NFLXA) XPDB,XKEF1
      DO 102 K=1,KMAX
      WRITE(NFLXA) ((P2(J,I,K),J=1,JMAX),I=1,IMAX)
  102 CONTINUE
      REWIND NFLXA
      IF (IX(73).LE.1) GO TO 108
      REWIND NFLXB
      READ(NFLXB) PRD,XK
      X = XPDB*XK/(PRD*XKEF1)
      ALPHA =-X*(XSRH1(1)-XKEF1)/(XSRH1(1)-XK)
  103 IF (ALPHA.GE.(-0.5)) GO TO 104
      ALPHA = (-0.5)
  104 CONTINUE
      DO 107 K=1,KMAX
      READ(NFLXB) ((SOUR(J,I),J=1,JMAX),I=1,IMAX)
      DO 106 I=1,IMAX
      DO 105 J=1,JMAX
      PN = P2(J,I,K)
      IF (PN.LE.0.0) GO TO 105
      PNP1 = PN + ALPHA*SOUR(J,I)
      IF (PNP1.LE.0.0) PNP1 = 1.0D-40
      P2(J,I,K) = PNP1
  105 CONTINUE
  106 CONTINUE
  107 CONTINUE
      REWIND NFLXB
      IF (NSRH(24).LT.0) WRITE(IOUT,1000) ALPHA,NFLXA,NFLXB
  108 CONTINUE
      IX(152) = NFLXA
      IX(153) = NFLXB
      IF (IX(130).GT.0.AND.IX(73).EQ.1) IX(73) = IX(73) + 1
      RETURN
 1000 FORMAT(' DB20',' ALPHA=',1PE15.6,2I10)
      END
