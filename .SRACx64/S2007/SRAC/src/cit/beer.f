CBEER --041 ***CITATION*** EDITS FIXED SOURCE              /CF - FXSO
C
      SUBROUTINE BEER(SPAR,JVX,IVX,KBVX)
C
      INCLUDE  'CITPMINC'
CDEL  INTEGER RGX , MSX , ZNEX , ZDX , WZX
CDEL  PARAMETER ( RGX=100, MSX=211, ZDX=200, ZNEX=1000, WZX=100 )
C
      COMMON/ALSUB/BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,
     & LMAX,MMAX, NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,
     & IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(100), IX(200),INNO(100),
     &  NGC(24),IEDG(24),ITMX(24),TIMX(6), GLIM(6),NDPL(24),IEDP1(24),
     & IEDP2(24),IEDP3(24), DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),
     & XSRH1(6), XTR1(WZX),XTR2(WZX),NXTR1(WZX),NXTR2(WZX),SPARE(200),
     & IXPUT(200),XPUT(200)
C
      DIMENSION SPAR(JVX,IVX,KBVX)
C
      IS1 = 0
      IS2 = 0
      DO 110 KB = 1,KBMAX
      IF (NUAC(5).GT.10) WRITE(IOUT,1000) KB
      N1J = JMAX
      N2J = 0
  100 N1I = IMAX
      N2I = 0
      IF (N1J-11) 101,101,102
  101 N1L = N2J+1
      N2J = JMAX
      IS1 = 1
      GO TO 103
  102 N1L = N2J+1
      N2J = N2J+11
      N1J = N1J-11
  103 CONTINUE
      IF (N1I-50) 104,104,105
  104 N2L = N2I+1
      N2I = IMAX
      IS2 = 1
      GO TO 106
  105 N2L = N2I+1
      N2I = N2I+50
      N1I = N1I-50
  106 WRITE(IOUT,1001)(J,J=N1L,N2J)
      DO 107 I = N2L,N2I
      WRITE(IOUT,1002)I,(SPAR (J,I,KB ),J=N1L,N2J)
  107 CONTINUE
      IF (IS2) 103,103,108
  108 IS2 = 0
      IF (IS1) 100,100,109
  109 IS1 = 0
  110 CONTINUE
      RETURN
 1000 FORMAT(1H0,' PLANE NUMBER',I3)
 1001 FORMAT(1H0,I10,10I11)
 1002 FORMAT(1H ,I3,1PE12.3,10E11.3)
      END
