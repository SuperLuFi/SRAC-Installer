CIPTM --006 ***CITATION*** INPUT MANAGER ROUTINE
C
      SUBROUTINE IPTM(A,ISTEP,MEMORY,NTITE,IMEM,NM1VX,NSM1VX,MM1AC5)
C
CDEL  INTEGER RGX , MSX , ZNEX , ZDX , WZX
CDEL  PARAMETER ( RGX=100, MSX=211, ZDX=200, ZNEX=1000, WZX=100 )
      INCLUDE  'CITPMINC'
C
      COMMON/MAINC/ IOPTS(120)
      EQUIVALENCE (IOPTS(55),NEF),(IOPTS(56),NET),(IOPTS(57),NERF),
     &            (IOPTS(58),NERT)
      COMMON/SRACIT/ID2,IXKI,IDELAY,IXYZ(ZNEX)
      COMMON/PERTCM/P(1),MEMPET,IPERT,ICASE,LSAMPL,LIOPT,LIDOPT,LBKLE,
     &              LXYZ,LBKLGP,LAST,LTERM,LSIG2,LSACS,LXI2,LBUF,NBUF
      COMMON /CRBNC/ ICONV,ICRBN,NCBSTP,AVZAB(ZNEX)
C     ICRBN IS SET IN CIT1 IF SRAC, OR CRBN IF COREBN
C     ICRBN =0 : SRAC-CITATION, ICRBN=1 : COREBN
C
      COMMON/ALSUB/BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,
     & LMAX,MMAX, NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,
     & IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(100), IX(200),INNO(100),
     &  NGC(24),IEDG(24),ITMX(24),TIMX(6), GLIM(6),NDPL(24),IEDP1(24),
     & IEDP2(24),IEDP3(24), DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),
     & XSRH1(6), XTR1(WZX),XTR2(WZX),NXTR1(WZX),NXTR2(WZX),SPARE(200),
     & IXPUT(200),XPUT(200)
      COMMON/AMESH/BMESH(30),NREGI,NREGJ,NREGKB,XSHI(RGX),XSHJ(RGX),
     & XSHKB(RGX), MSHI(RGX),MSHJ(RGX),MSHKB(RGX),Y(MSX),YY(MSX), X(MSX)
     &  ,XX(MSX),Z(MSX),ZZ(MSX), ZONVOL(ZNEX),AVZPD(ZNEX),PDI(MSX),
     & PDJ(MSX) , PDK(MSX)
      COMMON/AFLUX/BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,
     & ISTART,IEP, VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,
     & XLEK,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3, NI3,IEXTR,
     & IRECV,VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2, BETTA,SUMXI,IX25,IX28,I,J,
     &  KB,K,ITMAX,ITIME, BET(MSX),DEL(MSX)
      COMMON/ABURN/BBURN(30),NSIG1(50),NSIG2(50),NSIG3(50),N1N2R(2,ZDX),
     &  NSIG4(50),NSIG5(50),NSIG6(50),NJM(50),NJMM(50),NJNQ(50),NCH(50),
     &  NZON(ZDX),NXSET(ZDX),NXODR(ZDX),IDXSET(ZDX),NCLASS(ZDX),NDP(ZDX)
     &  , XNAME(3,ZDX)
      COMMON/AVDLM/IVDLM(1),IVX,JVX,KBVX,KVX,LVX,MVX,NVX,IVXP1,JVXP1,
     & KBVXP1,NSETVX,NVO,IVO,IVZ,KVZ,NCRP,NSPA,N3DDIM,NBLOCK, JIVX,
     & JIP1VX,JP1IXZ,IOVX,IOVZ
      COMMON/AKADD/KAY(1),K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11, K12,K13,
     & K131,K14,K15,K16,K17,K18,K19,K20,K21,K22,K23,K24, K25,K26,K27,
     & K28,K29,K30,K31,K32,K33,K34,K35,K36,K37, K38,K39,K40,K41,K42,K43,
     &  K44,K45,K46,K47,K48,K49, K50,K51,K52,K53,K54,K55,K56,K57,K58,
     & K59, K60,K61,K62, K63,K64,K65,K66,K67,K68,K69,K70,K71,K72,K73,
     & K74,K75, K76,K77,K78,K79,K80,K81,K82,K83,K84,K85,K86,K87, K88,
     & K89,K90, K91,K92,K93,K94,K95,K96,K97,K98, K99,K100,NDATA,KNRGN,
     & KNCOMP, KPVOL,KRVOL,MEMVRY, MEMX
      COMMON/ASRCH/ BSRCH(30),XK1,XK2,XK3,XN1,XN2,XN3, DELK1,DELK2,
     & DELK3,BATTY,DRV,TBF,GWC,EK2,RCCM,DNDK(5), NSC(5),NSCN,NXZ,NXN,
     & NXM,NXS,INIL,INIU,INID
      COMMON/CMARY/MEMARY,IMN,MNI,IJLMN,NMLJI,IY(50),AX(50),TITL(36)
      COMMON/COOPD/FLOTR(200),INTGR(100)
      REAL*8 FLOTR
C
      DIMENSION A(MEMORY)
C
      DIMENSION NTXR1(100)
C
C **  START OF PROCESS
C
      NUO    = IX(190)
      NHEX   = IX(191)
      NRVX   = IX(192)
      IJKBX  = IX(193)
      NTYP18 = INNO(18)
      DO 100 I = 1,100
      NER(I)   = 0
      INNO(I)  = 0
      NXTR1(I) = 0
  100 CONTINUE
      IF (IX(27).EQ.0) GO TO 101
      CALL SHOX(A(K30),A(K31),A(K32),A(K33),NVX,NSETVX)
      IF (NCRP.EQ.1) NSPA = 0
  101 CONTINUE
      NTYP = 0
      INCT = 0
  102 CONTINUE
      NTYP0 = NTYP
      I9193 = 0
      READ(IOIN,1003)NTYP
  103 CONTINUE
      INTGR(16) = 0
      INCT = INCT+1
      IF (NTYP-999) 104,192,105
  104 IF (NTYP) 105,107,107
  105 IERR = 1
  106 NER(IERR) = IERR
      INCT = INCT-1
      WRITE(IOUT,1004)(NTXR1(I),I=1,INCT),NTYP
      GO TO 206
  107 CONTINUE
      IF (NTYP.LT.NTYP0) GO TO 105
      IF (NTYP.NE.0) GO TO 108
      REWIND IO1
      WRITE(IO1)(A(I),I=1,MEMORY)
      END FILE IO1
      REWIND IO1
      IO30 = IX(97)
      IIIN = IOIN
      IIUT = IOUT
      II2 = IO2
      II3 = IO3
      II7 = IO7
      IISIG = IOSIG
      II30 = IO30
      CALL OPT1(A,MEMORY,IIIN,IIUT,II2,II3,II7,IISIG,II30)
      READ(IO1)(A(I),I=1,MEMORY)
      REWIND IO1
      READ(IOIN,1003)NTYP
      IX(167) = 0
      IF (NTYP.EQ.999) IX(167) = 1
      IF (NTYP.EQ.999) GO TO 209
      NTYP0 = NTYP
      GO TO 103
  108 CONTINUE
  109 INNO(NTYP) = NTYP
      NTXR1(INCT) = NTYP
      IF ((NTYP.EQ.91).OR.(NTYP.EQ.93)) GO TO 187
      GO TO (111,116,117,118,122,127,132,133,137,138,139,140,147,148,
     & 149,150,151,152,153,154,155,156,157,158,159,160,162,163,164,165,
     & 167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,
     & 183,184,185,186),NTYP
  110 WRITE(IOUT,1005) NTYP
      GO TO 208
  111 CALL CNTR(ISTEP)
C     ADJOINT FLUX PRINT OPTION
      IXPUT(32) = 0
      IF (NGC(12).LT.0) IXPUT(32) = 1
      NGC(12) = IABS(NGC(12))
      IF (NGC(2).EQ.0) GO TO 114
      CALL RSTR(NTITE,A,MEMORY)
      IX(27) = 1
      IF (NCRP.EQ.1) NSPA = 0
      REWIND IO2
      N1 = KNRGN+IVX*JVX*KBVX-1
      WRITE(IO2)(A(N),N=KNRGN,N1)
      END FILE IO2
      REWIND IO2
      IRV = 1
      CALL BNSB(A(K50),A(K51),KVX,IRV)
C*******************************(BBND,BND,KVX,IRV)
      IMEM = 1
      IF (NGC(19).GT.0) GO TO 102
      CALL KXNX
      IF ((NMAX.EQ.NVX).AND.(KMAX.EQ.KVX)) GO TO 112
      WRITE(IOUT,1000)NMAX,NVX,KMAX,KVX
      GO TO 208
  112 CONTINUE
      ISRTT = 1
      CALL TAPE(A(K9), A(K12), A(K13) ,A(K10),A(K1),A(K2), A(K3),A(K4),
     & A(K5),A(K6),A(K8),A(K7),A(K16), A(K30),A(K31),A(K32), A(K33),
     & A(K14),KVX,NVX,NSETVX,NRVX,ISRTT)
C**************(NJJR,NREFNO,NNDI1,NNDI2,NNXTRA,MJJR,SS1,SS2,SS3,
C ****SS4,SS5,SSC,HOL,ONEOV,XI,HOX,NFO,NNFO,NIC,SIG,KVX,NVX,NSETVX,NRVX)
      NUO = 0
      DO 113 M = 1,MMAX
      N1 = NZON(M)
      N2 = NXSET(M)
      N3 = NXODR(N2)
      N4 = NSIG2(N3)
      NUO = NUO+N1*N4
  113 CONTINUE
      IF (NTITE.LE.0) GO TO 102
      INTGR(16) = 1
      IO13 = IX(80)
      INTGR(13) = IO13
      INTGR(14) = NGC(2)
      INTGR(15) = NGC(3)
      IO12 = IX(79)
      INTGR(17) = IO12
      IO24 = IX(91)
      INTGR(18) = IO24
      I9193 = 1
      GO TO 187
  114 CONTINUE
      IF (IX(27).GT.0) GO TO 102
      DO 115 N = 1,MEMORY
      A(N) = 0.0
  115 CONTINUE
      GO TO 102
  116 CALL HIST
      IX(76) = 0
      IF (NGC(2).GT.0) IX(76) = 1
      GO TO 102
  117 CALL GEOMC(ISTEP,A(1))
      NHEX = 0
      IF ((NUAC(5).EQ.9).OR.(NUAC(5).EQ.13)) NHEX=1
      IF (IX(27).EQ.0) GO TO 102
      IVZ = IVX+NHEX
      KVZ = KVX*(NHEX+1)
      GO TO 102
  118 CALL LVMX
      IF (LVX.NE.LMAX) IX(7) = 1
      LVX = LMAX
      KRVOL = MEMORY-LVX+1
      KPVOL = KRVOL-LVX
      KNCOMP = KPVOL-LVX
      KNO = IABS(KNCOMP)
      IF (KNCOMP.LE.0) GO TO 207
      CALL MESH(ISTEP,A(KRVOL),A(KPVOL),LVX)
C**************(RVOL,PVOL,LVX)
      IVX = IMAX
      IVXP1 = IMXP1
      JVX = JMAX
      JVXP1 = JMXP1
      KBVX = KBMAX
      KBVXP1 = KBMXP1
      KNRGN = 1
      IF (IX(27).GT.0) KNRGN = K19
      IJKBX = IVX*JVX*KBVX
      ISK = 0
      IF (NUAC(5)-10) 119,119,120
  119 CALL COMP(ISTEP,ISK,A(KNRGN),A(KNCOMP),IVX,JVX,LVX)
C**************(ISK,NRGN,NCOMP,IVX,JVX,LVX)
      GO TO 121
  120 CALL KOMP(ISTEP,ISK,A(KNRGN),A(KNCOMP),IVX,JVX,KBVX,LVX)
C**************(ISK,NRGNE,NCOMP,IVX,JVX,KBVX,LVX)
  121 REWIND IO2
      N1 = KNRGN+IJKBX-1
      WRITE(IO2)(A(N),N=KNRGN,N1)
      END FILE IO2
      REWIND IO2
      GO TO 102
  122 ISK = 1
      IF (NUAC(5)-10) 123,123,124
  123 CALL COMP(ISTEP,ISK,A(KNRGN),A(KNCOMP),IVX,JVX,LVX)
C**************(ISK,NRGN,NCOMP,IVX,JVX,LVX)
      GO TO 125
  124 CALL KOMP(ISTEP,ISK,A(KNRGN),A(KNCOMP),IVX,JVX,KBVX,LVX)
C**************(ISK,NRGNE,NCOMP,IVX,JVX,KBVX,LVX)
  125 CONTINUE
      IF (NGC(2).EQ.0) GO TO 126
      IF (MMAX.EQ.MVX) GO TO 126
      WRITE(IOUT,1000)MMAX,MVX
      WRITE(IOUT,2000)
      GO TO 208
  126 MVX = MMAX
      GO TO 102
  127 CONTINUE
      IF (INNO(5).NE.5) GO TO 105
      N1 = NDATA+1+IJKBX
      NT1 = MEMORY-(N1+LVX)+1
      NT2 = (NT1-2*LVX)/3
      N2 = N1+LVX+NT2
      N3 = N2+LVX+NT2
      NCHECK = N3+LVX
      IF (NCHECK.LE.MEMORY) GO TO 128
      WRITE(IOUT,1006)NDATA,MEMORY,LVX,N1,NT1,NT2,N2,N3
      WRITE(IOUT,2001) NCHECK
      STOP
  128 CONTINUE
      DO 129 N = 1,LVX
      NN = N-1
      NN1 = KNCOMP+NN
      NN2 = N1+NN
      NN3 = KPVOL+NN
      NN4 = N2+NN
      NN5 = KRVOL+NN
      NN6 = N3+NN
      A(NN2) = A(NN1)
      A(NN4) = A(NN3)
      A(NN6) = A(NN5)
  129 CONTINUE
      NNN1 = N1+LVX-1
      LSZ = NT2+LVX
      CALL OVER(ISTEP,
     &          A(KNRGN),A(N1),A(N3),A(N2),IVX,JVX,KBVX,LSZ,NNN1,N2)
C**************(NRGN,NCOMP,RVOL,PVOL,IVX,JVX,KBVX,LSZ,NNN1,N2)
      REWIND IO2
      NLIM = KNRGN+IVX*JVX*KBVX-1
      WRITE(IO2)(A(N),N=KNRGN,NLIM)
      END FILE IO2
      REWIND IO2
      LVX = LMAX
      MVX = MMAX
      KRVOL = MEMORY-LVX+1
      KPVOL = KRVOL-LVX
      KNCOMP = KPVOL-LVX
      DO 130 N = 1,LVX
      NN = N-1
      NN1 = N1+NN
      NN2 = KNCOMP+NN
      NN3 = N2+NN
      NN4 = KPVOL+NN
      NN5 = N3+NN
      NN6 = KRVOL+NN
      A(NN2) = A(NN1)
      A(NN4) = A(NN3)
      A(NN6) = A(NN5)
      A(NN1) = 0.0
      A(NN3) = 0.0
      A(NN5) = 0.0
  130 CONTINUE
      IF (NUAC(5).GT.10) GO TO 131
      CALL CMOT(ISTEP,A(KNRGN),A(KNCOMP),IVX,JVX,LVX)
C**************(NRGN,NCOMP,IVX,JVX,LVX)
      GO TO 102
  131 CALL KMOT(ISTEP,A(KNRGN),A(KNCOMP),IVX,JVX,KBVX,LVX)
C**************(NRGNE,NCOMP,IVX,JVX,KBVX,LVX)
      GO TO 102
  132 CONTINUE
      GO TO 102
  133 READ(IOIN,1007)NSIG3(1),NSIG4(1),NSIG5(1)
      IF (NSIG3(1).GT.0) GO TO 134
      IX(136) = 1
      IOMC = IX(137)
CKSK FOR COREBN & SRAC
      IF(ICRBN.EQ.0.AND.ISTEP.EQ.1) GO TO 1135
CKSK  IF (ISTEP .EQ. 1) GO TO 135
      REWIND IOMC
      READ(IOMC,1007) NATE
      IF (NATE.NE.8) NER(35) = 35
C     READ(IOMC,1007) NSIG3(1),NSIG4(1),NSIG5(1)
      READ(IOMC,1007) NSIG3(1),NSIG4(1),NSIG5(1),ID2,IXKI,IDLAY1
      GO TO 135
  134 CONTINUE
      IX(136) = 0
      IOMC = IOIN
  135 CONTINUE
CKSK  IF (NGC(13).EQ.0) GO TO 1135
      IF (NGC(12).EQ.0) GO TO 1135
      IF (IDLAY1.NE.0.OR.IPERT.NE.0) GO TO 1135
      WRITE(IOUT,1014)
      STOP
CKSK
 1135 CONTINUE
      NLIM = KNRGN+IVX*JVX*KBVX-1
      DO 136 N = KNRGN,NLIM
      A(N) = 0.0
  136 CONTINUE
      NSIG1(1) = 0
      NSIG2(1) = 0
C     IF(ISTEP .EQ. 1) NSIG3(1)=IABS(NSIG3(1))
C     IF(ISTEP .EQ. 1) NSIG4(1)=1
C     IF(ISTEP .EQ. 1) NSIG5(1)=1
CKSK START
      IF(ICRBN.EQ.0) THEN
CMOD 97/10/20 FOR MACRO INPUT FROM STANDARD INPUT =======
C       IF(ISTEP .EQ. 1) THEN
        IF(ISTEP .EQ. 1 .AND. IX(136).EQ.1) THEN
CMOD END ======
          IF (IOPTS(2).EQ.5.AND.IOPTS(12).EQ.5) WRITE(IOUT,1015)
          NSIG3(1)=1
          NSIG4(1)=1
          NSIG5(1)=1
          KMAX = NEF
          IF (IOPTS(4).NE.0) KMAX = KMAX + NET
          IF (IOPTS(10).NE.0.AND.IOPTS(12).EQ.5) THEN
            KMAX = NERF
            IF (IOPTS(4).NE.0) KMAX=KMAX+NERT
          ENDIF
        ELSE
          KMAX = NSIG3(1)
        ENDIF
      ELSE
        KMAX = NSIG3(1)
      ENDIF
CKSK END
      NMAX = 1
      IX(28) = NSIG4(1)
      IX(29) = NSIG5(1)
      NVX = NMAX
      KVX = KMAX
      NSETMX = 1
      NSETVX = 1
      NRVX = 1
      CALL KSIG(KVX,MVX,NVX,NSETVX)
      KNO = K18-KNCOMP
      IF (KNO.GE.0) GO TO 207
      KBVSIG = K17 + NVX*MVX
      KDXII  = KBVSIG + KVX*IDELAY*MVX
      KBLSIG = KDXII  + KVX*IDELAY*MVX
CKSK START
C     IF(ISTEP.NE.1) CALL MACR(A(K14),A(K15),A(K16),MVX,KVX,IOMC)
C    & A(KBVSIG),A(KDXII))
      IF(ICRBN.EQ.0) THEN
CMOD 97/10/20 FOR MACRO INPUT FROM STANDARD INPUT =======
C       IF(ISTEP.NE.1) CALL MACR(A(K14),A(K15),A(K16),MVX,KVX,IOMC,
C    &  A(KBVSIG),A(KDXII),A(KBLSIG))
        IF(ISTEP.NE.1 .OR. IX(136).EQ.0)
     &                 CALL MACR(A(K14),A(K15),A(K16),MVX,KVX,IOMC,
     &  A(KBVSIG),A(KDXII),A(KBLSIG))
CMOD END ======
      ELSE
        CALL MACR(A(K14),A(K15),A(K16),MVX,KVX,IOMC,
     &  A(KBVSIG),A(KDXII),A(KBLSIG))
      ENDIF
CKSK END
C**************(SIG,F1,XI,MVX,KVX,IOMC)
      IF (IX(136).GT.0) REWIND IOMC
      NUO = 1
      NVO = 1
      IVO = 1
      IVZ = IVX+NHEX
      KVZ = KVX*(NHEX+1)
      CALL KRST(IVX,JVX,KBVX,KVX,LVX,MVX,NVX,IVXP1,JVXP1, KBVXP1,NSETVX,
     &  NVO,IVO,IVZ,KVZ,N3DDIM)
      IMEM = 1
      KNO = K62-KNCOMP
      IF (KNO.GE.0) GO TO 207
      IF (NGC(19).EQ.0) NGC(19) = 1
      GO TO 102
  137 CONTINUE
      GO TO 102
  138 CONTINUE
      GO TO 102
  139 CONTINUE
      GO TO 102
  140 CONTINUE
      IF (NGC(19).GT.0) GO TO 142
      NLIM = KNRGN+IVX*JVX*KBVX-1
      DO 141 N = KNRGN,NLIM
      A(N) = 0.0
  141 CONTINUE
  142 CONTINUE
      CALL SSET
      NSETVX = NSETMX
      IF (NGC(19).GT.0) NSETMX = 1
      IF (NGC(19).GT.0) NSETVX = 1
      IF (NGC(19)) 143,143,102
  143 CALL KXNX
      NVX = NMAX
      KVX = KMAX
      CALL KSIG(KVX,MVX,NVX,NSETVX)
      KNO = K18-KNCOMP
      IF (KNO.GE.0) GO TO 207
      NN = K14-1
      DO 144 N = 1,NN
      A(N) = 0.0
  144 CONTINUE
      NN = K34-1
      DO 145 N = K30,NN
      A(N) = 0.0
  145 CONTINUE
      ISRTT = 0
      CALL TAPE(A(K9), A(K12), A(K13) ,A(K10),A(K1),A(K2), A(K3),A(K4),
     & A(K5),A(K6),A(K8),A(K7),A(K16), A(K30),A(K31),A(K32), A(K33),
     & A(K14),KVX,NVX,NSETVX,NRVX,ISRTT)
C**************(NJJR,NREFNO,NNDI1,NNDI2,NNXTRA,MJJR,SS1,SS2,SS3,
C ****SS4,SS5,SSC,HOL,ONEOV,XI,HOX,NFO,NNFO,NIC,SIG,KVX,NVX,NSETVX,NRVX)
      IF (NTYP18.EQ.0) CALL TAPX(A(K9),NVX,NSETVX)
C******************************(NJJR,NVX,NSETVX)
      NUO = 0
      IVO = 0
      DO 146 M = 1,MMAX
      N1 = NZON(M)
      N2 = NXSET(M)
      N3 = NXODR(N2)
      N4 = NSIG2(N3)
      N1N4 = N1*N4
      IVO = MAX0(IVO,N1N4)
      NUO = NUO+N1N4
  146 CONTINUE
      NVO = IVO
      IVZ = IVX+NHEX
      KVZ = KVX*(NHEX+1)
      CALL KRST(IVX,JVX,KBVX,KVX,LVX,MVX,NVX,IVXP1,JVXP1, KBVXP1,NSETVX,
     &  NVO,IVO,IVZ,KVZ,N3DDIM)
      IMEM = 1
      KNO = K62-KNCOMP
      IF (KNO.GE.0) GO TO 207
      N1 = MEMORY-K18
      KNO = NUO-N1
      INSEC = -36
      REWIND IO3
      CALL DISK(A(K17),A(K18),INSEC,MVX,NVX,NVO, A(K30),A(K31),A(K32),
     & A(K33),NSETVX)
      IF (KNO.GT.0) GO TO 207
      GO TO 102
  147 CONTINUE
      GO TO 102
  148 CONTINUE
      GO TO 102
  149 CONTINUE
      GO TO 102
  150 CONTINUE
      GO TO 102
  151 CONTINUE
      GO TO 102
  152 CALL CLAS
      GO TO 102
  153 CONTINUE
      GO TO 102
  154 IDTOR = 0
      CALL DENS(A(K17),A(K18),A(K10),A(K9),IDTOR,MVX,NVX,NSETVX,NVO,NUO)
C**************(CONC,ZONEN,MJJR,NJJR,IDTOR,MVX,NVX,NSETVX,NVO,NTO)
      GO TO 102
  155 CONTINUE
      GO TO 102
  156 CONTINUE
      GO TO 102
  157 CONTINUE
      GO TO 102
  158 CONTINUE
      CALL BKLE(A(K14),MVX,KVX)
C**************(SIG,MVX,KVX)
      GO TO 102
  159 CONTINUE
      GO TO 102
  160 CONTINUE
      N1 = JVX*IVX*KBVX
      N2 = K62 + N1
      N3 = N2 + N1
      KNO = N3 - KNCOMP
      IF (KNO.GE.0) GO TO 207
      N4 = N3 - 1
      REWIND IO2
      READ(IO2) (A(N),N=N2,N4)
      REWIND IO2
      CALL FXSO(A(K62),A(K45),A(KPVOL),A(N2 ),A(KNCOMP), JVX,IVX,KBVX,
     & KVX,LVX)
C**************(SPAR  ,BIEMS ,PVOL    ,NRGNE   ,NCOMP    ,
C***** JVX,IVX,KBVX,KVX,LVX)
      DO 161 N=N2,N4
  161 A(N) = 0.0
      NSPA = 1
      IX(131) = 1
      GO TO 102
  162 CONTINUE
      GO TO 102
  163 INSEC = 28
      REWIND IO3
      CALL DISK(A(K17),A(K18),INSEC,MVX,NVX,NVO, A(K30),A(K31),A(K32),
     & A(K33),NSETVX)
C**************(CONC,   ZONEN,INSEC,MVX,NVX,KVX,NVO)
      CALL SRCH(A(K17),A(K10),NVX,MVX,NSETVX)
C**************(CONC,MJJR,NVX,MVX,NSETVX)
      GO TO 102
  164 CONTINUE
      GO TO 102
  165 CONTINUE
      INSEC = 30
      REWIND IO3
      CALL DISK(A(K17),A(K18),INSEC,MVX,NVX,NVO, A(K30),A(K31),A(K32),
     & A(K33),NSETVX)
      N1 = JVX*IVX*KBVX
      N2 = K62 + N1*NSPA
      N3 = N2 + N1
      KNO = N3 - KNCOMP
      IF (KNO.GE.0) GO TO 207
      N4 = N3 - 1
      REWIND IO2
      READ(IO2) (A(N),N=N2,N4)
      REWIND IO2
      CALL RODI(A(N2),A(KNCOMP),JVX,IVX,KBVX,LVX,A(K10),NSETVX)
C*************(NRGNE,NCOMP,JVX,IVX,KBVX,LVX,MJJR,NSETVX)
      DO 166 N=N2,N4
  166 A(N) = 0.0
      IX(142) = 1
      GO TO 102
  167 CONTINUE
      GO TO 102
  168 CONTINUE
      GO TO 102
  169 CONTINUE
      GO TO 102
  170 CALL DCAY(A(K8),A(K9),A(K10),NVX,NSETVX)
C**************(HOL,NJJR,MJJR,NVX,NSETVX)
      CALL YELD(A(K30),A(K9),A(K10),A(K31),A(K32),NVX,NSETVX)
C**************(HOX,NJJR,MJJR,NFO,NNFO,NVX,NSETVX)
      GO TO 102
  171 CONTINUE
      GO TO 102
  172 CALL CHAN(A(K8),A(K10),A(K30),A(K31),A(K32),A(K33),NVX,NSETVX)
C**************(HOL,MJJR,HOX,NFO,NNFO,NIC,NVX,NSETVX)
      GO TO 102
  173 CONTINUE
      GO TO 102
  174 CONTINUE
      CALL IMXS(A(K50),KVX)
      GO TO 102
  175 CONTINUE
      GO TO 102
  176 INSEC = 40
      REWIND IO3
      CALL DISK(A(K17),A(K18),INSEC,MVX,NVX,NVO, A(K30),A(K31),A(K32),
     & A(K33),NSETVX)
C**************(CONC,   ZONEN,INSEC,MVX,NVX,KVX,NVO)
      CALL IPRT(A(K17),A(K18),A(K10),A(K9),A(K8),MVX,NVX,NSETVX,NVO,NUO)
C**************(CONC,ZONEN,MJJR,NJJR,HOL,MVX,NVX,NSETVX,NVO,NTO)
      GO TO 102
  177 CONTINUE
      GO TO 102
  178 CALL DYPD
      GO TO 102
  179 CONTINUE
      GO TO 102
  180 CONTINUE
      GO TO 102
  181 CONTINUE
      GO TO 102
  182 CONTINUE
      GO TO 102
  183 CONTINUE
      GO TO 102
  184 CONTINUE
      GO TO 102
  185 CONTINUE
      GO TO 102
  186 CONTINUE
      GO TO 102
  187 CONTINUE
      REWIND IO1
      WRITE(IO1)(A(I),I=1,MEMORY)
      END FILE IO1
      REWIND IO1
      INTGR(11) = 1
      INTGR(100) = NTYP
      CALL GEDT(A,MEMORY,IO3)
      IF (INTGR(16).EQ.0) GO TO 189
      INTGR(16) = 0
      NGC(2) = INTGR(14)
      IX(19) = NGC(2)
      IOR = IX(80)
      IO12 = IX(79)
      REWIND IO12
      IF (NGC(2).GT.0) GO TO 189
      READ(IOR)II
      IX19 = II
      IX22 = IX(22)
      IX2 = IX(2)
      IX39 = IX(39)
      IX198 = IX(198)
      READ(IOR)N12,(IX(I),I=1,200),(SPARE(I),I=1,200), (A(I),I=K17,N12)
      IX(22) = IX22
      IX(19) = IX19
      IX(2) = IX2
      IX(39) = IX39
      IX(198) = IX198
      BACKSPACE IOR
      BACKSPACE IOR
      NZN2 = 0
      WRITE(IO12)MMAX,MMAX,MMAX,MMAX,MMAX
      INCR = K18-1
      DO 188 M = 1,MMAX
      NACT = NXSET(M)
      MS1 = NXODR(NACT)
      NZN1 = NZN2+1
      NZN2 = NZN2+NJM(MS1)*NZON(M)
      J1 = NZN1+INCR
      J2 = NZN2+INCR
      WRITE(IO12)NZN1,NZN2
      WRITE(IO12)(A(I),I=J1,J2)
  188 CONTINUE
      END FILE IO12
      REWIND IO12
      IX(27) = 1
  189 CONTINUE
      READ(IO1)(A(I),I=1,MEMORY)
      REWIND IO1
      IF (I9193.EQ.0) GO TO 102
      IF (NGC(2).GT.0) GO TO 190
      GO TO 102
  190 CONTINUE
      READ(IO12)
      DO 191 M = 1,MVX
      READ(IO12)
      READ(IO12)
  191 CONTINUE
       IEND=K17+NMAX*MMAX-1
      READ(IO12)(A(I),I=K17,IEND)
      REWIND IO12
      IX19 = IX(19)
      IX22 = IX(22)
      IX2 = IX(2)
      IX39 = IX(39)
      IX198 = IX(198)
      READ(IO13)(IX(I),I=1,200),(SPARE(I),I=1,200)
      IX(19) = IX19
      IX(22) = IX22
      IX(2) = IX2
      IX(39) = IX39
      IX(198) = IX198
      IX(27) = 1
      GO TO 102
  192 CONTINUE
      IF (NGC(19).GT.0) GO TO 194
      IF (IX(27).EQ.0) GO TO 193
      IF ((INNO(34).EQ.34).OR.(INNO(36).EQ.36)) GO TO 193
      GO TO 194
  193 CONTINUE
      CALL WIO3(A(K30),A(K31),A(K32),A(K33),NVX,NSETVX)
      INSEC = 36
      CALL DISK(A(K17),A(K18),INSEC,MVX,NVX,NVO, A(K30),A(K31),A(K32),
     & A(K33),NSETVX)
  194 CONTINUE
      IF (NGC(2).EQ.0) GO TO 195
      IO26 = IX(93)
      REWIND IO26
C
C     WRITE(6,*) ' ** IO26 K24 N11 ** ',IO26,K24,N11
C
      READ(IO26) N11,(A(I),I=K24,N11)
      REWIND IO26
  195 CONTINUE
      IF (IX(27).EQ.0) GO TO 196
      IF (IMEM.GT.0) GO TO 196
      IVZ = IVX+NHEX
      KVZ = KVX*(NHEX+1)
      CALL KSIG(KVX,MVX,NVX,NSETVX)
      KNO = K18-KNCOMP
      IF (KNO.GE.0) GO TO 207
      CALL KRST(IVX,JVX,KBVX,KVX,LVX,MVX,NVX,IVXP1,JVXP1, KBVXP1,NSETVX,
     &  NVO,IVO,IVZ,KVZ,N3DDIM)
      KNO = K62-KNCOMP
      IF (KNO.GE.0) GO TO 207
  196 CONTINUE
      IRV = 0
      IF (NGC(19).EQ.0) CALL CPNC
      IF (KVX.GT.1000) STOP 445
      CALL BNSB(A(K50),A(K51),KVX,IRV)
C*******************************(BBND,BND,KVX,IRV)
C     NSPA = NUMBEROFSPACEPOINTARRAYS-----
C            0FORSTATICSORDEPLETIONPROBLEMS.
C            1FORFIXED-SOURCEPROBLEMS.
C            5FORXENONPROBLEMS.
C            2TIMESTHENUMBEROFDELAYED-NEUTRONGROUPS
C            PLUS5FORDYNAMICSPROBLEMS.
      IF (NGC(10).NE.(-5)) GO TO 197
      IF (IX(131).EQ.1) GO TO 197
      IERR = 50
      GO TO 206
  197 CONTINUE
      NCRP = IVX*JVX*KBVX
      IF (NSPA.EQ.0) NCRP = 1
      IF (NSPA.EQ.0) NSPA = 1
      K64 = K62+NCRP*NSPA
      NBLOCK = 2200
      CALL CNIO(ISTEP,
     &          IVX,JVX,KBVX,KVX,IVXP1,JVXP1,KBVXP1,IVZ,KVZ, N3DDIM,LVX,
     &  NBLOCK,IOVX,IOVZ)
      IF (IX(37).EQ.0) GO TO 198
      WRITE(IOUT,1001)
      GO TO 199
CKSK START
C 198 IF(ISTEP .NE. 1) WRITE(IOUT,1002)
  198 IF(ICRBN.EQ.0) THEN
        IF(ISTEP .NE. 1) WRITE(IOUT,1002)
      ELSE
        WRITE(IOUT,1002)
      ENDIF
CKSK END
  199 CONTINUE
      NDATA = K68
      KNRGN = NDATA+1
      N1 = KNRGN+IVX*JVX*KBVX
      KNO = N1-KNCOMP
      IF (KNO.GE.0) GO TO 207
      N2 = N1-1
      READ(IO2)(A(N),N=KNRGN,N2)
      REWIND IO2
CKSK START
C 200 IF(ISTEP .NE. 1)
C    &WRITE(IOUT,1008)JMAX,IMAX,KBMAX,KMAX,IX(29),IX(28),LMAX,MMAX
  200 IF(ICRBN.EQ.0) THEN
        IF(ISTEP .NE. 1)
     &  WRITE(IOUT,1008)JMAX,IMAX,KBMAX,KMAX,IX(29),IX(28),LMAX,MMAX
      ELSE
        WRITE(IOUT,1008)JMAX,IMAX,KBMAX,KMAX,IX(29),IX(28),LMAX,MMAX
      ENDIF
CKSK END
      NUSE = NDATA+IVX*JVX*KBVX+3*LVX
      MEMX = MEMORY-NUSE
C ***
C ***  PERTURBATION MACRO READ BUFFER
      LBUF = NUSE
      NBUF = MEMX
      WRITE(IOUT,1009)MEMORY,NUSE,MEMX
      CALL SIZE(IVX,JVX,KBVX,KVX,MVX,NVX,NSETVX,NRVX)
      IF (IX(27).EQ.0) GO TO 201
      IF (NGC(2).NE.0) GO TO 201
      IF ((NUAC(5).NE.MM1AC5).AND.(INNO(4).EQ.0)) NER(28) = 28
      IF ((MM1VX.EQ.MMAX).AND.(KM1VX.EQ.KMAX).AND.(NM1VX.EQ.NMAX)
     & .AND.(NSM1VX.EQ.NSETVX)) GO TO 201
      IF ((INNO(8).EQ.0).AND.(NGC(19).GT.0)) NER(25) = 25
      IF ((NGC(19).EQ.0).AND.(INNO(20).EQ.0)) NER(25) = 25
      IF (INNO(24).EQ.0) NER(25) = 25
  201 N1 = 0
      IF ((IX(7).EQ.1).AND.(INNO(5).EQ.0)) NER(19) = 19
      DO 203 I = 1,100
      IF (NER(I)) 203,203,202
  202 N1 = NER(I)
      WRITE(IOUT,1010)N1
      IF (N1.EQ.19) WRITE(IOUT,2002)
      IF (N1.EQ.25) WRITE(IOUT,2003)
      IF (N1.EQ.28) WRITE(IOUT,2004)
      IF (N1.EQ.35) WRITE(IOUT,2005)
  203 CONTINUE
      IF (N1) 204,204,208
  204 CONTINUE
      IF (IX(27)) 205,205,209
  205 N1 = INNO(1)+INNO(3)+INNO(4)+INNO(5)
      N2 = N1*INNO(8)
      N3 = N1*INNO(12)*INNO(20)
      N = N2+N3
      IF (N) 105,105,209
  206 WRITE(IOUT,1010)IERR
      IF (IERR.EQ.1) WRITE(IOUT,2006)
      IF (IERR.EQ.50) WRITE(IOUT,2007)
      GO TO 208
  207 WRITE(IOUT,1013)KNO
      WRITE(IOUT,1008)JMAX,IMAX,KBMAX,KMAX,IX(29),IX(28),LMAX,MMAX
  208 STOP
  209 CONTINUE
      IF (NGC(2).EQ.0.AND.NDPL(11).GT.0) CALL RODO(A(KNCOMP),LVX)
      IX(190) = NUO
      IX(191) = NHEX
      IX(192) = NRVX
      IX(193) = IJKBX
      IX(150) = 0
      IF (NGC(5).NE.0.AND.NGC(1).EQ.0) IX(150) = 1
      IF (IPERT.EQ.0.OR.ISTEP.EQ.1) RETURN
      CALL PERTCK(P(LSAMPL),P(LIOPT) ,P(LXYZ)  ,P(LXYZ)  ,XX       ,
     &            YY       ,ZZ       ,A(KNRGN) ,A(KNCOMP),
     &            ICASE    ,JVXP1    ,IVXP1    ,KBVXP1   ,MMAX     ,
     &            IOUT     ,JMAX     ,IMAX     ,KBMAX    ,IX(25)   ,
     &            LMAX                                              )
      RETURN
 1000 FORMAT(1H0,'ERROR STOP 27',4I5)
 1001 FORMAT(1H0/
     & 1H0,'EQUATION CONSTANTS WILL BE STORED ON I/O LOGICAL 15')
 1002 FORMAT(1H0/1H0,'EQUATION CONSTANTS WILL BE STORED IN CORE')
 1003 FORMAT(24I3)
 1004 FORMAT('0THE FOLLOWING INPUT SECTIONS HAVE BEEN READ'/1H ,32I4/
     & 1H ,32I4)
 1005 FORMAT(1H0,'INPUT SECTION NUMBER',I6,'    IS IN ERROR')
 1006 FORMAT(1H0,'NCHECK',8I8)
 1007 FORMAT(24I3)
 1008 FORMAT(1H0,'NUMBER OF---COLUMNS, ROWS, PLANES, GROUPS,',
     & 1H ,'UPSCAT, DOWNSCAT, REGIONS, AND ZONES    ',8I5)
 1009 FORMAT(1H0,'MEMORY LOCATIONS RESERVED FOR DATA STORAGE---',I7/
     & 1H ,'MEMORY LOCATIONS USED FOR THIS PROBLEM-------',I7/
     & 1H ,'MEMORY LOCATIONS NOT USED--------------------',I7)
 1010 FORMAT(1H0,'ERROR STOP NUMBER',I3)
 1011 FORMAT(1H0/'0**************************************************'
     &,'*************************************************************')
 1012 FORMAT('0**************************************************'
     &,'*************************************************************')
 1013 FORMAT(1H0,'ERROR STOP NUMBER 2'/
     & 1H ,'MEMORY RESERVED FOR DATA EXCEEDED BY MORE THAN',I7,' WORDS')
C1014 FORMAT('0ERROR STOP IN CITATION , NGC(13).NE.0 , BUT DELAYED ',
 1014 FORMAT('0ERROR STOP IN CITATION , NGC(12).NE.0 , BUT DELAYED ',
     & 'NEUTRON DATA DOES NOT EXIST IN MACRO FILE'/
     & ' OR PERTURBATION CALCULATION OPTION IS NOT SELECTED'        )
 1015 FORMAT(' WARNNING ]] FIXED SOURCE PROBLEM AND EIGENVALUE PROBLEM',
     &       ' IS MULTI DIMENSIONAL CITATION USED'/
     &       ' GROUP DEPENDENT INPUT DATA IS ASSUMED IN EIGENVALUE STE',
     &       'P' )
 2000 FORMAT(' THE RESTART DEVISE, LOGICAL 13 DOSE NOT CONTAINS PROPER',
     &      ' RESTART DATA SUCH DATA AS NUMBER OF ZONE IS INCONSISTENT')
 2001 FORMAT(' MEMORY RESERVED FOR DATA EXCEEDED AT MESH OVERLAY INPU',
     &       'T STEP  NEEDED MEMORY IS MORE THAN',I7,' WORDS'         )
 2002 FORMAT(' IN A SUCCEEDING DATA, INPUT SECTION 004 CHANGED THE NUM',
     &       'BER OF REGIONS AND INPUT SECTION 005 WAS NOT INCLUDED.')
 2003 FORMAT(' IF THE NUMBER OF ENERGY GROUPS, ZONES, OR MICROSCOPIC ',
     &       'CROSS SECTION SETS DIFFER FROM A PREVIOUS CASE, THEN '/
     &       'INPUT SECTIONS 020 MUST BE INCLUDED FOR THIS CASE'     )
 2004 FORMAT(' WITH A GEOMETORY CHANGE, INPUT SECTION 004 MUST BE INCL',
     &       'UDED WITH THE SUCCEEDING CASES.')
 2005 FORMAT(' MACRO SCOPIC CROSS SECTIONS HAVE BEEN INPROPERLY REQUES',
     &       'TED FROM I/O DEVICE.'                                    )
 2006 FORMAT(' DATA OUT OF ORDER; SECTION NUMBER ERROR; NOT ENOUGH DAT',
     &       'A FOR FIRST CASE.'                                       )
 2007 FORMAT(' SECTION 026 MUST BE SUPPLIED FOR A FIXED SOURCE PROBLEM.'
     &      )
      END