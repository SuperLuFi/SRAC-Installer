CSETV --024 ***CITATION*** ZEROS CERTAIN VARIABLES/ CF-IPTM
C
      SUBROUTINE SETV
C
CDEL  INTEGER RGX , MSX , ZNEX , ZDX , WZX
CDEL  PARAMETER ( RGX=100, MSX=211, ZDX=200, ZNEX=1000, WZX=100 )
      INCLUDE  'CITPMINC'
      INTEGER IJX , JKX , KLX , LMX
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
      COMMON/ASRCH/ BSRCH(30),XK1,XK2,XK3,XN1,XN2,XN3, DELK1,DELK2,
     & DELK3,BATTY,DRV,TBF,GWC,EK2,RCCM,DNDK(5), NSC(5),NSCN,NXZ,NXN,
     & NXM,NXS,INIL,INIU,INID
C
      COMMON/CMARY/MEMARY,IMN,MNI,IJLMN,NMLJI,IY(50),AX(50),TITL(36)
C
      IJX = 1341 + WZX*4
CM100 DO 101 N = 1,1741
  100 DO 101 N = 1, IJX
      BLSUB(N) = 0.0
  101 CONTINUE
      JKX  = 33 + RGX * 6 + MSX * 9 + ZNEX * 2
CM    DO 102 N = 1,4532
CM    DO 102 N = 1,2932
      DO 102 N = 1,JKX
      BMESH(N) = 0.0
  102 CONTINUE
      KLX  = 76 + 2 * MSX
CM    DO 103 N = 1,498
      DO 103 N = 1,KLX
      BFLUX(N) = 0.0
  103 CONTINUE
      LMX  = 530  + 11 * ZDX
CM    DO 104 N = 1,2730
      DO 104 N = 1,LMX
      BBURN(N) = 0.0
  104 CONTINUE
      DO 105 N=1,63
      BSRCH(N) = 0.0
  105 CONTINUE
      MEMARY = 0
      IMN = 0
      MNI = 0
      IJLMN = 0
      NMLJI = 0
      DO 106 N = 1,136
      IY(N) = 0
  106 CONTINUE
      IOIN = 91
      IOUT = 99
      IOSIG = 8
      IOFLX = 9
      IO1 = 1
      IO2 = 2
      IO3 = 3
      IO4 = 4
      IO7 = 7
      IX(77) = 10
      IO10 = IX(77)
      IX(78) = 11
      IO11 = IX(78)
      IX(79) = 12
      IO12 = IX(79)
      IX(80) = 13
      IO13 = IX(80)
      IX(81) = 14
      IX(82) =15
      IX(83) = 16
      IX(84) = 17
      IX(85) = 18
      IX(86) = 19
      IX(87) = 20
      IX(88) = 21
      IX(89) = 22
      IX(90) = 23
      IX(91) = 24
      IX(92) = 25
      IX(93) = 26
      IX(94) = 27
      IX(95) = 28
      IX(96) = 29
      IX(97) = 30
      IX(137) = 31
      IX(138) = 32
      IX(139) = 33
      IX(140) = 34
      IX(141) = 35
      N1 = 1000
      N2 = 0
      REWIND IO10
      WRITE(IO10) N1,N2,N2,N2,N2
      END FILE IO10
      REWIND IO10
      WRITE(IOUT,1000)
      RETURN
 1000 FORMAT(1H1,22X,'**********CITATION - REVISION 2 (JULY 1971) - ',
     &'SUPPLEMENT 3 (JULY 1972)**********')
      END