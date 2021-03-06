CKSIG --052 ***CITATION*** CALCULATES A(K1-K18)/ CF-IPTM
C
      SUBROUTINE KSIG(KVX,MVX,NVX,NSETVX)
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
      COMMON/AKADD/KAY(1),K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11, K12,K13,
     & K131,K14,K15,K16,K17,K18,K19,K20,K21,K22,K23,K24, K25,K26,K27,
     & K28,K29,K30,K31,K32,K33,K34,K35,K36,K37, K38,K39,K40,K41,K42,K43,
     &  K44,K45,K46,K47,K48,K49, K50,K51,K52,K53,K54,K55,K56,K57,K58,
     & K59, K60,K61,K62, K63,K64,K65,K66,K67,K68,K69,K70,K71,K72,K73,
     & K74,K75, K76,K77,K78,K79,K80,K81,K82,K83,K84,K85,K86,K87, K88,
     & K89,K90, K91,K92,K93,K94,K95,K96,K97,K98, K99,K100,NDATA,KNRGN,
     & KNCOMP, KPVOL,KRVOL,MEMORY, MEMX
C
      COMMON /SRACIT/ ID2,IXKI,IDELAY,IXYZ(ZNEX)
C
  100 K1 = 1
      KNNSX = KVX*NVX*NSETVX
      K2 = K1+KNNSX
      K3 = K2+KNNSX
      K4 = K3+KNNSX
      K5 = K4+KNNSX
      K6 = K5+KNNSX
      K7 = K6+KVX*KVX
      K8 = K7+KVX*NSETVX
      K9 = K8+NVX*NSETVX*10
      NNSX = NVX*NSETVX
      K10 = K9+NNSX
      K12 = K10+NSETVX*200
      K13 = K12+NNSX
      K14 = K13 +NNSX
C     K15 = K14 + KVX*MVX*10
      K15 = K14 + KVX*MVX*12
CI85/07/24 FOR NEUTRON DENSITY EDIT
      IF (IEDG(16).NE.0) K15 = K15 + KVX*MVX
      K16 = K15 + KVX*MVX
C     K17 = K16+KVX
      K17 = K16+KVX*MVX
C     DELAYED NEUTRON DATA STORAGE
      IF (NGC(12).EQ.0) GO TO 10
      IF (IDELAY.EQ.0) GO TO 10
      KBVSIG = K17+NVX*MVX
      KDXII = KBVSIG + KVX*IDELAY*MVX
      KBLSIG = KDXII + KVX*IDELAY*MVX
      K18=KBLSIG+KVX*IDELAY*MVX
      GO TO 20
   10 CONTINUE
      K18 = K17+NVX*MVX
   20 CONTINUE
C
C     OVERLAID STORAGE
C
      K30=K18+NVX*IX(168)+2*KVX*MVX
      K31=K30+20*NVX*NSETVX
      K32=K31+NSETVX*20
      K33=K32+NSETVX*20
      K34=K33+1000
C
      RETURN
      END
