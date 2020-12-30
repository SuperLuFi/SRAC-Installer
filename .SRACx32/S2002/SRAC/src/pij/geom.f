C             GEOM                LEVEL=10       DATE=84.02.08
      SUBROUTINE GEOM(RX,RPP,RDP,NPIN,THETA,NTR,NAMEP,NAMER,DD,IM,IP)
      DIMENSION RX(1),NPIN(1),RDP(1),RPP(1),THETA( 1),NAMER(1)
     &         ,NTR(1),NAMEP(2,NDPIN,1),DD(1),IM(1),IP(1)
      COMMON / PIJ1C / NX,NY,NTPIN,NAPIN,IDUM(4),NDPIN,IDUM1(7),IXP,
     &                 IYP,IZP,NDPIN1,NDR,NDA,LL,I24(4),RAN,I29,
     &                 X,BETA
      COMMON  /PIJ2C/ IPIJ2(10),NTTAB
      LL=0
      K=NX
    1 DIST=RX(K+1)**2-X**2
      IF(DIST.GT.0.) GO TO 2
      IF(X.GT.RAN) GO TO 210
      GO TO 100
    2 DCP=SQRT(DIST)
      DCM=-DCP
      IZ=NAMER(K)
C9000 FORMAT(1H ,8HGEOM 222,2X,2HK=,I5,2X,9HNAMER(K)=,I5)
      CALL INSERT(DCP,IZ          ,IZ          ,DCM,DD,IM,IP)
      K=K-1
      IF(K.GT.0) GO TO 1
C
  100 CONTINUE
C     IF(IPIJ2(9).GT.1 .AND. MOD(IPIJ2(8),10).EQ.0)  THEN
C     WRITE(6,*) IPIJ2(8),' LL=',LL,' RHO=',X,' ANG=',BETA
C     WRITE(6,9030) (IM(L),IP(L),DD(L),L=1,LL)
C     END IF
      NN=0
      DO 200 I=1,NAPIN
      NTRR=NTR(I)
      IF(NTRR.EQ.0) GO TO 102
      DIST= RX(NTRR)**2-X**2
      IF(DIST.LT.0.)   GO TO 101
      DCP=SQRT(DIST)
      DCM=-DCP
      GO TO 102
  101 NTRR=0
  102 CONTINUE
      JMAX=NPIN(I)
      DO 170 J=1,JMAX
      NN=NN+1
      TH=THETA(NN)
      DIST=(RPP(I)*COS(BETA-TH)-X)**2
      TM1= RPP(I)*SIN(TH-BETA)
      N=NDPIN1
  103 DET=DIST-RDP(N)**2
      IF( DET.GE.0.) GO TO 170
      TM2 = SQRT(-DET)
      DSP = TM1 + TM2
      DSM = TM1 - TM2
      N=N-1
C9010 FORMAT(1H ,13HNAMEP(1,N,I)=,I5,2X,13HNAMEP(2,N,I)=,I5
C    1     ,2X,2HN=,I5,2X,2HI=,I5)
C
      IF(NTRR.EQ.0) GO TO 140
      IF(DSP.GT.DCP) GO TO 110
      IF(DCM.LT.DSM) GO TO 150
      IF(DCM.LT.DSP) GO TO 160
      GO TO 140
  110 IF(DSM.LT.DCM) GO TO 130
      IF(DSM.GT.DCP) GO TO 140
C
C                                       IDVP=2     IDVP=1
C   0    (DSP  DSM)                      2 2        1 1     140
C   1     DCP (DSP  DSM) DCM             1 1        1 1     150
C   2     DCP (DSP  DCM  DSM)            1 2        1 1     160
C   3     DCP  DCM (DSP  DSM)            2 2        1 1     140
C   4    (DSP (DCP  DCM) DSM)          2 1 1 2    1 1 1 1   130 140
C   5    (DSP  DCP  DSM) DCM             2 1        1 1     120
C   6    (DSP  DSM) DCP  DCM             2 2        1 1     140
C   A    (DSP 2  2  DSM)      140
C   B    (DSP 1  1  DSM)      150
C   C    (DSP 1  2  DSM)      160
C   D    (DCP 1  1  DCM)      130
C   E    (DSP 2  1  DSM)      120
  120 CALL INSERT(DSP,NAMEP(2,N,I),NAMEP(1,N,I),DSM,DD,IM,IP)
      GO TO 165
  130 CALL INSERT(DCP,NAMEP(1,N,I),NAMEP(1,N,I),DCM,DD,IM,IP)
  140 CALL INSERT(DSP,NAMEP(2,N,I),NAMEP(2,N,I),DSM,DD,IM,IP)
      GO TO 165
  150 CALL INSERT(DSP,NAMEP(1,N,I),NAMEP(1,N,I),DSM,DD,IM,IP)
      GO TO 165
  160 CALL INSERT(DSP,NAMEP(1,N,I),NAMEP(2,N,I),DSM,DD,IM,IP)
  165 CONTINUE
C     IF(IPIJ2(9).GT.2 .AND. MOD(IPIJ2(8),10).EQ.0) THEN
C     WRITE(6,*) IPIJ2(8),' LL=',LL,' RHO=',X,' ANG=',BETA
C    *,' J=',J,' NN=',NN,' NAMER=',NTRR,' DCP,DCM=',DCP,DCM,
C    * ' DSP,DSM=',DSP,DSM,' THETA=',TH
C     WRITE(6,9510) NAMEP(1,N,I),NAMEP(2,N,I),N,I
C9510 FORMAT(1H ,13HNAMEP(1,N,I)=,I5,2X,13HNAMEP(2,N,I)=,I5
C    1     ,2X,2HN=,I5,2X,2HI=,I5)
C     WRITE(6,9030) (IM(L),IP(L),DD(L),L=1,LL)
C     END IF
C9030 FORMAT(1H ,6HIM(I)=,I5,
C    1       2X,6HIP(I)=,I5,2X,6HDD(I)=,F11.5)
      IF(N.GT.1) GO TO 103
  170 CONTINUE
  200 CONTINUE
  210 LL = LL - 1
      DO 220 L = 1,LL
      DD(L) = DD(L) - DD(L+1)
  220 CONTINUE
      IXP=IXP+1
      RETURN
      END
