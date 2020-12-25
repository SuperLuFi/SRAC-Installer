      SUBROUTINE GEOMH(RX,RPP,RDP,THETA,TY,DD,IM,IP,
     *                 NTR,NAMEP,NAMER,NPINPS,NPINDV)
      DIMENSION RX(1),RDP(NDPIN1,1),RPP(1),THETA(1),NPINPS(1),NPINDV(1),
     1 NAMER(1),NTR(1),NAMEP(2,NDPIN,1),DD(1),IM(1),IP(1),TY(1)
      COMMON / PIJ1C / NX,NY,NTPIN,NAPIN,NCELL,NM,NGR,NGA,NDPIN,
     1                 IDIVP,BETM,NX1,NY1,I14,I15,I16,IXP,IYP,IZP,
     2                 NDPIN1,NDR,NDA,LL,L0,RO1,DRO,FVOL,RAN,
     3                 PIT,X,BETA,INV,NHEX,SINB,COSB,I36
      COMMON /PIJ2C/ IPIJ(10)
      DIMENSION SD(6)
      DATA C0,PI,P3 /1.7320508,3.141593,1.0471975 /
C
      EPS=.001*RX(NX1)
      LL=0
      K=NX
           IF (NHEX.EQ.1) THEN
C     HEXAGONAL
      NS1=BETA/P3
      ALP=BETA-NS1*P3
      C1=COS(ALP)
      C2=SIN(ALP)
      C3=2.*PIT
      CS=C0*C1
      CC=C0*C2
      SD(1)=(-X*(CC+C1)+C3)/(C2-CS)
      SD(2)=(X*(CC-C1)-C3)/(C2+CS)
      SD(3)=(-X*C1-PIT)/C2
      SD(4)=(X*(CC+C1)+C3)/(CS-C2)
      SD(5)=(X*(CC-C1)+C3)/(CS+C2)
      SD(6)=(PIT-X*C1)/C2
      DIN=AMIN1(SD(4),SD(5),SD(6))
      DOUT=SD(1)
      DO 35 I=1,3
      IF (SD(I)-DOUT) 35,34,34
   34 IJ=I
      DOUT=SD(I)
   35 NS1=NS1+IJ
      IF(NS1.GT.6) NS1=NS1-6
      IF (DIN.LE.DOUT) GO TO 680
      DCP=DIN
      DCM=DOUT
      GO TO 53
                  ENDIF
C ANNULAR
   51 D=RX(K+1)**2-X**2
      IF(D.LE.0.) GOTO 70
      DCP=SQRT(D)
      DCM=-DCP
   53 CONTINUE
CMM       IF(K.GT.1) THEN
      CALL SECT(NY,NSP,DCP,X,BETA,TY)
      CALL SECT(NY,NSM,DCM,X,BETA,TY)
      IA=NAMER(K)+NSP-1
      IB=NAMER(K)+NSM-1
CMM                  ELSE
CMM   IA=NAMER(1)
CMM   IB=NAMER(1)
CMM                  ENDIF
      CALL INSERT (DCP,IA,IB,DCM,DD,IM,IP)
      K=K-1
      IF(K.GT.0) GO TO 51
C
   70 CONTINUE
      IF(IPIJ(9).GT.2) THEN
      WRITE(6,*) ' RO=',X ,' ANG=',BETA
      WRITE(6,'(A,I2,A,10I12/(11X,10I12))')
     & ' 1 LL=',LL,'IM=',(IM(L),L=1,LL-1)
*     WRITE(6,'(A,I2,A,10I12/(11X,10I12))')
*    & '  IJK=',IJ ,'IP=',(IP(L),L=2,LL)
      WRITE(6,'(A,10F12.5/(13X,10F12.5))')
     &'        D(L)=',(DD(L),L=1,LL)
                      END IF
      IF(NY.LE.1) GOTO 310
C
      DO 300 NS=1,NY
      ARG=TY(NS)-BETA
      ARGSIN=SIN(ARG)
      ARGCOS=COS(ARG)
      IF(ARGSIN.EQ.0.) GOTO 300
      DIS=X*ARGCOS/ARGSIN
      R=-X/ARGSIN
*     DIS=-X*ARGCOS/ARGSIN
*     R=X/ARGSIN
      IF(R.LE.0.) GOTO 300
CMM   IF(R.LE.RX(2)) GOTO 300
CMM   DO 250 N=2,NX
      DO 250 N=1,NX
      KK=N
      IF(R.LT.RX(N+1)) GOTO 260
  250 CONTINUE
      GOTO 300
  260 CONTINUE
      IF(TY(1).EQ.0) THEN
      NSP=NS-1
      NSM=NS
      IF(NSP.EQ.0) NSP=NY
                     ELSE
      NSP=NS
      NSM=NS+1
      IF(NSM.EQ.NY+1) NSM=1
                     ENDIF
      IF(X.LT.0.)    THEN
      NTEMP=NSP
      NSP=NSM
      NSM=NTEMP
                     ENDIF
CMC   CALL SECT(NY,NSP,DIS+EPS,X,BETA,TY)
CMC   CALL SECT(NY,NSM,DIS-EPS,X,BETA,TY)
      IA=NAMER(KK)+NSP-1
      IB=NAMER(KK)+NSM-1
      IF(IPIJ(9).GT.2)
     &WRITE(6,*) ' K=',KK,' NS=',NS,' ANG=',ARG/PI*180.,' TY=',TY(NS)/PI
     & *180.    ,' R=',R,' DIS=',DIS,' NSP=',NSP,' NSM=',NSM
      CALL INTRP(DIS,IA,IB,DD,IM,IP)
  300 CONTINUE
      IF(IPIJ(9).GT.2) THEN
      WRITE(6,'(A,I2,A,10I12/(11X,10I12))')
     & ' 2 LL=',LL,'IM=',(IM(L),L=1,LL-1)
*     WRITE(6,'(A,I2,A,10I12/(11X,10I12))')
*    & '  IJK=',IJ ,'IP=',(IP(L),L=2,LL)
      WRITE(6,'(A,10F12.5/(13X,10F12.5))')
     &'        D(L)=',(DD(L),L=1,LL)
                       END IF
C
      IF(NTPIN.EQ.0) GO TO 510
  310 DO 500 I=1,NTPIN
      NTRR=NTR(I)
      IF(NPINDV(I).EQ.0) NTRR=0
      IF(NTRR.EQ.0) GO TO 360
      D= RX(NTRR)**2-X**2
      IF(D.LT.0.)   GO TO 350
      DCP=SQRT(D)
      DCM=-DCP
      GO TO 360
  350 NTRR=0
  360 CONTINUE
      TH=THETA(I)
      D=(RPP(I)*SIN(BETA-TH)-X)**2
      TM1=-RPP(I)*COS(TH-BETA)
      N=NDPIN1
  370 DET=D-RDP(N,I)**2
      N=N-1
      IF( DET.GE.0.) GO TO 500
      TM2 = SQRT(-DET)
      DSP = TM1 + TM2
      DSM = TM1 - TM2
      IF(IPIJ(9).GT.2 .AND. N.EQ.NDPIN)
     *               WRITE(6,*) ' PATH CROSSES    #',I,' TH PIN'
C
      IF(NTRR.EQ.0)                        THEN
C         ** NOT CROSS BY RPP   2 -> 2
                     IGO=  3
                                           ELSE
        IF(DSP.LT.DCP)                 THEN
          IF(DCM.LT.DSM)           THEN
C         ** DCP>(DSP>DSM)>DCM   1->1
                     IGO=  4
                                   ELSE
C         ** DCM>DSM
            IF(DCM.LT.DSP)    THEN
C         ** DCP>DSP>DCM>DSM   1 -> 2
                              IGO=  5
                              ELSE
C         ** DCP>DCM>(DSP>DSM)   2 -> 2
                              IGO=  3
                              ENDIF
                                   ENDIF
                                       ELSE
C         ** DSP>DCP
          IF(DSM.LT.DCM)           THEN
C        ** (DSP>(DCP>DCM)>DSM)   2 -> 1 -> 1 -> 2
                                   IGO=  2
                                   ELSE
C         ** DSP>DCP  & DSM>DCM
            IF(DSM.GT.DCP)    THEN
C         ** (DSP>DSM)>DCP>DCM 2 -> 2
                              IGO=  3
                              ELSE
C         ** (DSP>DCP>DSM)>DCM   2 -> 1
                              IGO=  1
                              ENDIF
                                   ENDIF
                                       ENDIF
                                           ENDIF
C
C     IF(NTRR.EQ.0) GO TO 150      IGO=3
C     IF(DSP.GT.DCP) GO TO 110
C     IF(DCM.LT.DSM) GO TO 150     IGO=4
C     IF(DCM.LT.DSP) GO TO 160     IGO=5
C     GO TO 140                    IGO=3
C 110 IF(DSM.LT.DCM) GO TO 130     IGO=2
C     IF(DSM.GT.DCP) GO TO 140     IGO=3
C     OTHERWISE                    IGO=1
C                                       IDVP=2     IDVP=1
C    3   (DSP  DSM)                      2 2        1 1     150
C    4    DCP (DSP  DSM) DCM             1 1        1 1     140
C    5    DCP (DSP  DCM  DSM)            1 2        1 1     160
C    3    DCP  DCM (DSP  DSM)            2 2        1 1     150
C    2   (DSP (DCP  DCM) DSM)          2 1 1 2    1 1 1 1   130
C    1   (DSP  DCP  DSM) DCM             2 1        1 1     120
C    3   (DSP  DSM) DCP  DCM             2 2        1 1     150
      GO TO (120,130,150,140,160),IGO
  120 CALL INSET7(DSP,NAMEP(2,N,I),NAMEP(1,N,I),DSM,DD,IM,IP)
      GO TO 170
  130 CALL INSET7(DSP,NAMEP(2,N,I),NAMEP(2,N,I),DSM,DD,IM,IP)
      CALL INSET7(DCP,NAMEP(1,N,I),NAMEP(1,N,I),DCM,DD,IM,IP)
      GO TO 170
  140 CALL INSET7(DSP,NAMEP(1,N,I),NAMEP(1,N,I),DSM,DD,IM,IP)
      GO TO 170
  150 CALL INSET7(DSP,NAMEP(2,N,I),NAMEP(2,N,I),DSM,DD,IM,IP)
      GO TO 170
  160 CALL INSET7(DSP,NAMEP(1,N,I),NAMEP(2,N,I),DSM,DD,IM,IP)
C **  RENAME IF INTERSECT TY
  170 IF(NPINDV(I).LT.2)            GO TO 380
      IF(NPINPS(I).GT.0)            GO TO 380
C     IF(N.LT.NDPIN1)               GO TO 380
      ARG=TY(-NPINPS(I))-BETA
      ARGSIN=SIN(ARG)
      ARGCOS=COS(ARG)
      IF(ARGSIN.EQ.0.)              GO TO 380
      DIS=X*ARGCOS/ARGSIN
      IF(DSP.LT.DIS)                GO TO 380
      IF(DSM.GT.DIS)                GO TO 380
      R=-X/ARGSIN
CMM   IF(R.LE.RX(2))                GO TO 380
      IF(R.LE.0.   )                GO TO 380
C     IF(RPP(I).EQ.0.)              GO TO 380
C     IF(R.LT.RPP(I)-RDP(N+1,I))    GO TO 380
C     IF(R.GT.RPP(I)+RDP(N+1,I))    GO TO 380
      IF(R.GT.RPP(I)) THEN
          NAME=NAMEP(2,N,I)
                      ELSE
          NAME=NAMEP(1,N,I)
                      ENDIF
      CALL INTRP(DIS,NAME,NAME,DD,IM,IP)
C     CALL REMOVE(DIS,DD,IM,IP)
  380 CONTINUE
      IF(IPIJ(9).GT.2) THEN
      WRITE(6,'(A,I2,A,10I12/(11X,10I12))')
     & ' 3 LL=',LL,'IM=',(IM(L),L=1,LL-1)
      WRITE(6,'(A,I2,A,10I12/(11X,10I12))')
     & '  IJK=',IJ ,'IP=',(IP(L),L=2,LL)
      WRITE(6,'(A,10F12.5/(13X,10F12.5))')
     &'        D(L)=',(DD(L),L=1,LL)
                       END IF
      IF(N.GT.1) GO TO 370
  500 CONTINUE
  510 IF(IPIJ(9).GT.2) THEN
      WRITE(6,'(A,I2,A,10I12/(11X,10I12))')
     & ' 4 LL=',LL,'IM=',(IM(L),L=1,LL-1)
      WRITE(6,'(A,I2,A,10I12/(11X,10I12))')
     & '  IJK=',IJ ,'IP=',(IP(L),L=2,LL)
      WRITE(6,'(A,10F12.5/(13X,10F12.5))')
     &'        D(L)=',(DD(L),L=1,LL)
                       END IF
      LL=LL-1
      DO 520 L=1,LL
      DD(L)=DD(L)-DD(L+1)
  520 CONTINUE
      IF (NHEX.EQ.0) GO TO 620
C   EXIT
      GO TO (610,640,630),IJ
  610 X=-PIT*(CC+C1)+X
  620 IXP=IXP+1
      GO TO 670
  640 X=-PIT*(CC-C1)+X
      IYP=IYP+1
      GO TO 670
  630 X=2.*PIT*C1+X
      IZP=IZP+1
  670 IF(INV.EQ.0) GOTO 680
      IF(NS1.EQ.1 .OR. NS1.EQ.4) GOTO 680
      X=-X
      BETA=BETA+PI
      IF(BETA.GT.2.*PI) BETA=BETA-2.*PI
  680 RETURN
      END
