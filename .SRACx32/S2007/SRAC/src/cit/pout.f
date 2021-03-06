CPOUT --137 ***CITATION*** PRINT 1,2-D FLUXES/ CF-OUTC
C
      SUBROUTINE POUT(P2 ,UTIL ,IND,SOURE,IVX,JVX,KBVX,KVX)
C
C     IND = 0---FOWARD FLUX OUTPUT
C     IND = 1---POINT POWER OUTPUT (PRINT & FILE(IO32))
CADD  IND =-1---POINT POWER OUTPUT (FILE(IO32) BUT NO PRINT)
C     IND = 2---POINT NEUTRON DENSITY OUTPUT
C     IND = 3---POINT NEUTRON ABSORPTION RATE IN A NUCLIDE OUTPUT
C     IND = 4---ADJOINT FLUX OUTPUT
C     IND = 5---IMPORTANCE MAP OF UNIT CHANGE IN NUCLIDE
C     IND = 6---RELATIVE IMPORTANCE MAP FOR NUCLIDE
C     IND = 7---IMPORTANCE MAP OF 1/V CROSS SECTION
C     IND = 8---CUMULATIVE HEAT GENERATION RATE
C     IND = 9---TEMPERATURE OR POWER COEFFICIENT
C     IND = 10--DAMAGE RATE
C
CDEL  INTEGER RGX , MSX , ZNEX , ZDX , WZX
CDEL  PARAMETER ( RGX=100, MSX=211, ZDX=200, ZNEX=1000, WZX=100 )
      INCLUDE  'CITPMINC'
C
      REAL*8 P2
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
      DIMENSION P2(JVX,IVX,KVX),UTIL(JVX,IVX,KBVX)
      DIMENSION SOURE(JVX,IVX,KBVX)
C
      IF(IND.EQ.-1) GO TO 150
  100 IS1 = 0
      IS2 = 0
      IPG = 0
      IF (IND.EQ.9) GO TO 104
      IX50 = 0
      N3 = 3
      IF ((IND.EQ.0).OR.(IND.EQ.4)) GO TO 101
      N = IMAX*JMAX
      IF ((IMAX.GT.25).OR.(N.GT.550)) GO TO 101
      IX50 = 1
      IF ((IMAX.LE.15).AND.(JMAX.LE.11)) N3 = 4
      IF (IX(50).GT.0) GO TO 102
  101 CONTINUE
      WRITE(IOUT,1001)(TITL1(I),I=1,18)
      WRITE(IOUT,1002)(TITL2(I),I=1,18)
      IF (IX50.EQ.0) GO TO 103
  102 CONTINUE
      IX(50) = IX(50)+1
      IF (IX(50).EQ.N3) GO TO 101
  103 CONTINUE
      IF (IX(50).EQ.N3) IPG = 1
      IF (IX(50).EQ.N3) IX(50) = 0
      KLAX = KMAX
      IF ((IND.EQ.0).OR.(IND.EQ.4)) GO TO 105
  104 CONTINUE
      KLAX = 1
  105 DO 136 K = 1,KLAX
      IF (IND.EQ.9) GO TO 106
      WRITE(IOUT,1003)
  106 CONTINUE
      IF (IND.NE.8) GO TO 113
      NGO = NGC(21)
      IF (NGO.EQ.0) NGO = 1
      GO TO (107,108,109,110,111,112),NGO
  107 WRITE(IOUT,1004)
      GO TO 122
  108 WRITE(IOUT,1005)
      GO TO 122
  109 WRITE(IOUT,1006)
      GO TO 122
  110 WRITE(IOUT,1007)
      GO TO 122
  111 WRITE(IOUT,1008)
      GO TO 122
  112 WRITE(IOUT,1009)
      GO TO 122
  113 CONTINUE
      IF (IND.EQ.9) GO TO 122
      IF (IND.EQ.7) GO TO 115
      IF (IND.EQ.6) GO TO 114
      IF (IND.NE.5) GO TO 116
      WRITE(IOUT,1010)IXPUT(34),SPARE(91)
      GO TO 122
  114 WRITE(IOUT,1011)IXPUT(35)
      GO TO 122
  115 WRITE(IOUT,1012)
      GO TO 122
  116 CONTINUE
      IF (IND.NE.4) GO TO 117
      WRITE(IOUT,1013)K
      GO TO 122
  117 CONTINUE
      NUD = IX(165)
      IF (IND.EQ.10) WRITE(IOUT,1000)NUD
      IF (IND.EQ.10) GO TO 122
      IF (IND.LT.3) GO TO 118
      WRITE(IOUT,1014)IX(117)
      GO TO 122
  118 CONTINUE
      IF (IND-1) 120,121,119
  119 WRITE(IOUT,1015)
      GO TO 122
  120 WRITE(IOUT,1016)K
      GO TO 122
  121 WRITE(IOUT,1017)
  122 CONTINUE
      N1J = JMAX
      N2J = 0
  123 N1I = IMAX
      N2I = 0
      IF (N1J-11) 124,124,125
  124 N1L = N2J+1
      N2J = JMAX
      IS1 = 1
      GO TO 126
  125 N1L = N2J+1
      N2J = N2J+11
      N1J = N1J-11
  126 CONTINUE
      IF (N1I-50) 127,127,128
  127 N2L = N2I+1
      N2I = IMAX
      IS2 = 1
      GO TO 129
  128 N2L = N2I+1
      N2I = N2I+50
      N1I = N1I-50
  129 WRITE(IOUT,1018)(J,J=N1L,N2J)
      DO 133 I = N2L,N2I
      IF (IND.EQ.9) GO TO 132
      IF ((IND.EQ.5).OR.(IND.EQ.6).OR.(IND.EQ.7).OR.(IND.EQ.8)) GO TO
     & 132
      IF ((IND.EQ.0).OR.(IND.EQ.4)) GO TO 131
  130 WRITE(IOUT,1019)I,(UTIL(J,I,1),J=N1L,N2J)
      GO TO 133
  131 WRITE(IOUT,1019)I,(P2 (J,I, K),J=N1L,N2J)
      GO TO 133
  132 CONTINUE
      WRITE(IOUT,1019)I,(SOURE(J,I,1 ),J=N1L,N2J)
  133 CONTINUE
      IF (IS2) 126,126,134
  134 IS2 = 0
      IF (IS1) 123,123,135
  135 IS1 = 0
  136 CONTINUE
      IF (IPG.EQ.0) GO TO 137
      WRITE(IOUT,1001)(TITL1(I),I=1,18)
      WRITE(IOUT,1002)(TITL2(I),I=1,18)
  137 CONTINUE
C     NOTE SEQUENCING 2000 - 2200 FOLLOWING
      IF(NGC(7).EQ.0) GO TO 400
      IF((IND.EQ.1).OR.(IND.EQ.8)) GO TO 150
      GO TO 400
  150 CONTINUE
      IF((NGC(7).EQ.1).AND.(IND.EQ.8)) GO TO 400
      IO32 = IX(138)
      READ(IO32)
      IJDG14 = IEDG(14)+IXPUT(14)
      IF((NGC(7).GT.1).AND.(IND.EQ.8)) GO TO 200
      WRITE(IO32) SPARE(12),SPARE(100),XKEF1
      WRITE(IO32)(((UTIL (J,I,KB),J=1,JMAX),I=1,IMAX),KB=1,KBMAX)
      END FILE IO32
      REWIND IO32
      GO TO 400
  200 CONTINUE
      IF(IJDG14.EQ.0) WRITE(IO32) SPARE(12),SPARE(100),XKEF1
      IF(IJDG14.EQ.0) GO TO 300
      READ(IO32)
      READ(IO32)
  300 CONTINUE
      WRITE(IO32)(((SOURE(J,I,KB),J=1,JMAX),I=1,IMAX),KB=1,KBMAX)
      END FILE IO32
      REWIND IO32
  400 CONTINUE
      RETURN
 1000 FORMAT('0POINT DAMAGE RATE (DISPLACEMENT/CC-SEC) FOR NUCLIDE',I5)
 1001 FORMAT(1H1,18A4)
 1002 FORMAT(1H ,18A4)
 1003 FORMAT(1H0)
 1004 FORMAT('0CUMULATIVE HEAT GENERATION RATE(MWT/CM**2) - FLOW FROM ',
     &'LEFT TO RIGHT')
 1005 FORMAT('0CUMULATIVE HEAT GENERATION RATE(MWT/CM**2) - FLOW FROM ',
     &'RIGHT TO LEFT')
 1006 FORMAT('0CUMULATIVE HEAT GENERATION RATE(MWT/CM**2) - FLOW FROM ',
     &'TOP TO BOTTOM')
 1007 FORMAT('0CUMULATIVE HEAT GENERATION RATE(MWT/CM**2) - FLOW FROM ',
     &'BOTTOM TO TOP')
 1008 FORMAT('0CUMULATIVE HEAT GENERATION RATE(MWT/CM**2) - FLOW FROM ',
     &'FRONT TO BACK')
 1009 FORMAT('0CUMULATIVE HEAT GENERATION RATE(MWT/CM**2) - FLOW FROM ',
     &'BACK TO FRONT')
 1010 FORMAT('0IMPORTANCE MAP FOR UNIT CHANGE (DELTA-K/K DELTA-N) IN N',
     &'UCLIDE',I3,'  VOLUME INTEGRAL IS',1PE13.5)
 1011 FORMAT('0RELATIVE IMPORTANCE MAP(N DELTA-K/K DELTA-N) FOR NUCLID',
     &'E',I3)
 1012 FORMAT(1H0,'IMPORTANCE MAP OF 1/V CROSS SECTION')
 1013 FORMAT(1H0,' GROUP',I3,' ADJOINT FLUX')
 1014 FORMAT('0POINT NEUTRON ABSORPTION RATE(N/CC-SEC) IN NUCLIDE',I4)
 1015 FORMAT(1H0,' POINT NEUTRON DENSITY (N/CC)')
 1016 FORMAT(1H0,' GROUP',I3,' FLUX')
 1017 FORMAT(1H0,' POINT POWER DISTRIBUTION (WATTS/CC)')
 1018 FORMAT(1H0,I10,10I11)
 1019 FORMAT(1H ,I3,1PE12.3,10E11.3)
      END
