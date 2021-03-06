CKRST --053 ***CITATION*** CALCULATES A(K19-K62)/ CF-INPT
C
      SUBROUTINEKRST(IVX,JVX,KBVX,KVX,LVX,MVX,NVX, IVXP1,JVXP1,KBVXP1,
     & NSETVX,NVO,IVO,IVZ,KVZ,N3DDIM)
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
  100 N3DDIM = IVX*JVX*KBVXP1*KVX
      IF (KBVX.EQ.1) N3DDIM = 1
      JIKBX = JVX*IVX*KBVX
      JDP = JIKBX
C    THE STEP ON THE NEXT CARD IS FOR DOUBLE PRECISION, IBM/360
C           REMOVE THE CARD IF IT DOES NOT APPLY ON YOUR MACHINE
      IF (KBVX.EQ.1) JDP = 2*JDP
      LKX = LVX*KVX
      MKX = MVX*KVX
      K38=K18+NVO
      K39 = K38 + MKX
      K19=K39+MKX
C    THE STEP ON THE NEXT CARD IS FOR DOUBLE PRECISION
      K19=2*(K19/2)+1
      K20=K19+JDP
      K23 = K20+JDP
      N1 = MKX*KVX
      N2 = KVX*(NSETVX+2)*KVX
      N3 = MAX0(N1,N2)
      K24 = K23+N3
C    THE STEP ON THE NEXT CARD IS FOR DOUBLE PRECISION
      K24 = 2*(K24/2)+1
      JIKBKX = JIKBX*KVX
      JDP = JIKBKX
C    THE STEP ON THE NEXT CARD IS FOR DOUBLE PRECISION
      IF (KBVX.EQ.1) JDP = 2*JDP
      K41 = K24+JDP
C
C     OVERLAID STORAGE
C
      K54 = K34+NVX
      K55 = K54+NVX
      K56 = K55+NVX
      K57 = K56+NVX
      K58 = K57+NVX
      K59 = K58+NVX
      K60 = K59+IVO
      K61 = K60+IVO
C
      K42 = K41+JIKBX
      NT1 = NVX*MVX*10
      K42 = MAX0(K42,K19+NT1,K61+IVO)
      K43 = K42+1
      K44 = K43+NVX*6
      K45 = K44+MVX*10
      K46 = K45+KVX
      K47 = K46+KVX
      K48 = K47+KVX
      K49 = K48+KVX*20
C    THE STEP ON THE NEXT CARD IS FOR DOUBLE PRECISION
      K49 = 2*(K49/2)+1
      KVXD = KVX
C    THE STEP ON THE NEXT CARD IS FOR DOUBLE PRECISION
      KVXD = 2*KVX
C     K50 = K49+KVXD
      K50 = K49+KVXD*MVX
      K51 = K50+KVX
      K6X = KVX*6
      K52 = K51+K6X
      K62 = K52 + K6X
      RETURN
      END
