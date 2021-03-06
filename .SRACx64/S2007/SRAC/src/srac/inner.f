C             INNER               LEVEL=1        DATE=81.11.14
      SUBROUTINE INNER ( C,Q,FLUX,BR1,BR2,BT1,BT2,FLUXA,COSMU,COSETA,
     &AL1,AL2,ALFL,B1,F,AB,FU,FD,FR,FL,QQ,WETA,WMU,WGT,P1,P2,P3,P4,A1,
     &A2,A3,A4,A5,IDX,IDY,IDYA,IDCS,NN,YH,SOURCE,CTOT,XDF,YDF,IHM,MT,NM,
     &IT,JT,MM,IM,JM,IP,JP )
C
      COMMON /TW1C/ DDD(1),LIM1,IA(210)
      COMMON /WORK/AAA(1),LIM2
      DIMENSION D(212),A(132)
      EQUIVALENCE (D(1),DDD(1)),(AAA(1),A(1))
      EQUIVALENCE (A(3),BA),(A(4),BC),(D(108),IPSO),(D(130),IPHAF),
     &(D(131),IPVAF),(A(5),J),(D(128),I6),(A(6),J1),(A(7),J2),
     &(D(132),LTHAF),(D(109),LTSO),(D(133),LTVAF),(D(158),NAFLUX)
C
      DIMENSION C(IHM,MT),Q(NM,IT,JT),FLUX(NM,IT,JT),FLUXA(IT,JT),
     &BR1(JT,MM),BR2(JT,MM),COSMU(1),COSETA(1),BT1(IT,MM),AL1(1),AL2(1),
     &BT2(IT,MM),ALFL(NN,IT),B1(1),F(IM,JM),AB(IM,JM),QQ(IM,JM),
     &FU(IM,JP),FD(IM,JP),FR(IP,JM),FL(IP,JM),WETA(1),WMU(1),WGT(1),
     &P1(NM,MM),P2(NM,MM),P3(NM,MM),P4(NM,MM),A1(1),A2(1),A3(1),A4(1),
     &A5(1),IDCS(1),IDX(1),IDY(1),IDYA(1),YH(1),SOURCE(NM,IT,JT)
      DIMENSION CTOT(IT,JT), XDF(1), YDF(1)
      DIMENSION NA(1)
C
C     MATERIAL MESH INDEXING
C
CKSK  EQUIVALENCE (D(1),NA(1))
      EQUIVALENCE (DDD(1),NA(1))
      EQUIVALENCE (IA(175),LDXC),(IA(176),LDYC)
C
      EQUIVALENCE (IA(7),IBL),(IA(8),IBR),(IA(9),IBB),(IA(10),IBT),
     &(IA(17),IHS),(IA(21),IQB),(IA(26),IITL),(IA(31),IEDOPT),
     &(IA(33),IQR),(IA(34),IQT),(IA(44),EPSI),(IA(74),ISCP),
     &(IA(77),ITP),(IA(78),JTP),(A(68),LQR1),(A(69),LQR2),(A(70),LQT1),
     &(A(71),LQT2),(A(93),LHA),(A(94),LGA),(A(111),LANF),(A(117),LQB1),
     &(A(118),LQB2),(A(8),JCONV),(A(20),IITNO),(A(21),TS),(A(22),IG),
     &(A(33),ZZ),(A(34),BB),(A(35),CC),(A(36),DD),(A(37),T),(A(38),QT),
     &(A(39),CT)
      EQUIVALENCE (A(74),LBT1),(A(40),SUMMU),(A(41),SUMETA),(A(42),AA),
     &(A(43),TI),(A(44),TJ),(A(45),TM),(A(47),ERR),(A(120),LBT3),
     &(A(121),LBT4),(D(84),NLIMIT),(A(49),IFLAG)
CKH
CKH   WRITE(6,600)
CK600 FORMAT(1H0,20(1H*),' CHECKING OUTPUT ----- INNER ',20(1H*))
CKH   WRITE(6,610) (NA(KKK),KKK=1,400)
CKH   WRITE(6,620) (D(KKK),KKK=1,400)
CK610 FORMAT(20(1X,I5))
CK620 FORMAT(10E13.5)
CKH
C
C     RESTORE SOURCE TO GROUP / STORE OLD FLUX / ZERO NEW FLUX /
C
  100 CONTINUE
      CALL REED (0,IPSO,Q,LTSO,1)
      DO 150 J=1,JT
      DO 150 I=1,IT
      IZ=NA(LDXC+I-1)+NA(LDYC+J-1)
      IC=IDCS(IZ)
      FLUXA(I,J)=FLUX(1,I,J)
      T1=XDF(I)*YDF(J)
      IF (IC.LT.0) GO TO 110
      SOURCE(1,I,J)=(Q(1,I,J)+C(IHS,IC)*T1*FLUX(1,I,J))*A5(I)
      GO TO 130
  110 K=1
      DO 120 N=1,ISCP
      T=(2*N-1)*C(IHS,N-1-IC)*T1
      DO 120 L=1,N
      SOURCE(K,I,J)=(Q(K,I,J)+T*FLUX(K,I,J))*A5(I)
  120 K=K+1
  130 DO 140 N=1,NM
  140 FLUX(N,I,J)=0.
  150 CONTINUE
C
C     PREPARE REBALANCE ARRAYS
C
      DO 170 J=1,JM
      DO 160 I=1,IM
      F(I,J)=1.0
  160 AB(I,J)=0.
      DO 170 I=1,IP
      FR(I,J)=0.
  170 FL(I,J)=0.
      DO 180 J=1,JP
      DO 180 I=1,IM
      FU(I,J)=0.
  180 FD(I,J)=0.
C
C     INDEX FOR ANGULAR FLUX STORAGE
C
      LAFS=LANF+ITP*MM
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C     CALCULATE FLUX IN DOWNWARD DIRECTIONS
C
C     SET TOP BOUNDARY CONDITION
C
CKSK  CALL SETBC ( BT1,BT2,A(LQT1),A(LQT2),WETA,MM,IT,IBT,IQT,SUMETA,
CKSK &A(LBT3),A(LBT4),FD,IDX,A3,IM,JP,JP,0 )
      CALL SETBC(BT1,BT2,AAA(LQT1),AAA(LQT2),WETA,MM,IT,IBT,IQT,SUMETA,
     &AAA(LBT3),AAA(LBT4),FD,IDX,A3,IM,JP,JP,0 )
C
C     STORE VERTICAL ANGULAR FLUX
C
CKSK  IF (JCONV.EQ.2) CALL STORAF (BT1,A(LQT1),IQT,JTP,IPVAF,LTVAF)
      IF (JCONV.EQ.2) CALL STORAF (BT1,AAA(LQT1),IQT,JTP,IPVAF,LTVAF)
C
C     SET RIGHT BOUNDARY CONDITION
C
CKSK  CALL SETBC ( BR1,BR2,A(LQR1),A(LQR2),WMU,MM,JT,IBR,IQR,SUMMU,
CKSK &A(LBT3),A(LBT4),FL,IDYA,YH,IP,JM,0,A4(ITP) )
      CALL SETBC(BR1,BR2,AAA(LQR1),AAA(LQR2),WMU,MM,JT,IBR,IQR,SUMMU,
     &AAA(LBT3),AAA(LBT4),FL,IDYA,YH,IP,JM,0,A4(ITP) )
C
C     BEGIN SWEEP IN
C
      DO 230 JB=1,JT
      J=JTP-JB
      J1=IDYA(J)
      J2=IDYA(J-1)
      BA=2.0*B1(J)
      BC=YH(J)
CKSK  IF (JCONV.EQ.2) CALL SAVEAF ( A(LANF),BR1,A(LQR1),IQR,ITP,J,ITP,
CKSK &JT,MM )
      IF (JCONV.EQ.2) CALL SAVEAF ( AAA(LANF),BR1,AAA(LQR1),IQR,ITP,J,
     &ITP,JT,MM )
      CALL IN ( SOURCE(1,1,J),FLUX(1,1,J),BR1,BT1,ALFL,P1,WGT,COSMU,
     &COSETA,WMU,WETA,AL1,AL2,A1,A2,A3,A4,CTOT(1,J),FL(1,J1),FD(1,J1),
     &IDX,NM,IT,JT,MM,NN,AAA(LANF),ITP )
CKSK &IDX,NM,IT,JT,MM,NN,A(LANF),ITP )
C
C     SET LEFT BOUNDARY CONDITION
C
      IF (IBL.NE.0) GO TO 200
      DO 190 M=1,MM
  190 BR1(J,M)=0.0
      GO TO 220
  200 K=IDYA(J)
      TA=A4(1)*YH(J)
      DO 210 M=1,MM
  210 FR(1,K)=FR(1,K)+WMU(M)*BR1(J,M)*TA
C
C     BEGIN SWEEP OUT
C
  220 CONTINUE
CKSK  IF (JCONV.EQ.2) CALL SAVEAF (A(LAFS),BR1,A(LQR1),0,1,J,ITP,JT,MM)
      IF (JCONV.EQ.2)
     &    CALL SAVEAF (AAA(LAFS),BR1,AAA(LQR1),0,1,J,ITP,JT,MM)
      CALL OUT ( SOURCE(1,1,J),FLUX(1,1,J),BR1,BT2,ALFL,P2,WGT,COSMU,
     &COSETA,WMU,WETA,AL1,AL2,A1,A2,A3,A4,CTOT(1,J),FR(1,J1),FD(1,J1),
     &IDX,NM,IT,JT,MM,NN,AAA(LAFS),ITP )
CKSK &IDX,NM,IT,JT,MM,NN,A(LAFS),ITP )
      IF (JCONV.NE.2) GO TO 230
C
C     STORE ANGULAR FLUXES FOR ROW J
C
CKSK  CALL STORAF (BT1,A(LQT1),0,J,IPVAF,LTVAF)
      CALL STORAF (BT1,AAA(LQT1),0,J,IPVAF,LTVAF)
CKSK  CALL STORAF (A(LANF),A(LQT1),0,J,IPHAF,LTHAF)
      CALL STORAF (AAA(LANF),AAA(LQT1),0,J,IPHAF,LTHAF)
  230 CONTINUE
C
C     CALCULATE FLUX IN UPWARD DIRECTIONS
C
C     SET BOTTOM BOUNDARY CONDITION
C
CKSK  CALL SETBC ( BT1,BT2,A(LQB1),A(LQB2),WETA,MM,IT,IBB,IQB,SUMETA,
CKSK &A(LBT3),A(LBT4),FU,IDX,A3,IM,JP,1,0 )
      CALL SETBC(BT1,BT2,AAA(LQB1),AAA(LQB2),WETA,MM,IT,IBB,IQB,SUMETA,
     &AAA(LBT3),AAA(LBT4),FU,IDX,A3,IM,JP,1,0 )
C
C     STORE VERTICAL ANGULAR FLUX
C
CKSK  IF (JCONV.EQ.2) CALL STORAF ( BT1,A(LQB1),IQB,1,IPVAF+LTVAF*JTP,
      IF (JCONV.EQ.2) CALL STORAF ( BT1,AAA(LQB1),IQB,1,IPVAF+LTVAF*JTP,
     &LTVAF )
C
C     BEGIN SWEEP IN
C
      DO 280 J=1,JT
      J1=IDYA(J)
      J2=IDYA(J+1)
      BA=2.0*B1(J)
      BC=YH(J)
CKSK  IF (JCONV.EQ.2) CALL SAVEAF ( A(LANF),BR2,A(LQR2),IQR,ITP,J,ITP,
      IF (JCONV.EQ.2) CALL SAVEAF(AAA(LANF),BR2,AAA(LQR2),IQR,ITP,J,ITP,
     &JT,MM )
      CALL IN ( SOURCE(1,1,J),FLUX(1,1,J),BR2,BT1,ALFL,P3,WGT,COSMU,
     &COSETA,WMU,WETA,AL1,AL2,A1,A2,A3,A4,CTOT(1,J),FL(1,J1),FU(1,J1+1),
     &IDX,NM,IT,JT,MM,NN,AAA(LANF),ITP )
CKSK &IDX,NM,IT,JT,MM,NN,A(LANF),ITP )
C
C     SET LEFT BOUNDARY CONDITION
C
      IF (IBL.NE.0) GO TO 250
      DO 240 M=1,MM
  240 BR2(J,M)=0.0
      GO TO 270
  250 K=IDYA(J)
      TA=A4(1)*YH(J)
      DO 260 M=1,MM
  260 FR(1,K)=FR(1,K)+WMU(M)*BR2(J,M)*TA
C
C     BEGIN SWEEP OUT
C
  270 CONTINUE
CKSK  IF (JCONV.EQ.2) CALL SAVEAF (A(LAFS),BR2,A(LQR2),0,1,J,ITP,JT,MM)
      IF (JCONV.EQ.2)
     &   CALL SAVEAF (AAA(LAFS),BR2,AAA(LQR2),0,1,J,ITP,JT,MM)
      CALL OUT ( SOURCE(1,1,J),FLUX(1,1,J),BR2,BT2,ALFL,P4,WGT,COSMU,
     &COSETA,WMU,WETA,AL1,AL2,A1,A2,A3,A4,CTOT(1,J),FR(1,J1),FU(1,J1+1),
     &IDX,NM,IT,JT,MM,NN,AAA(LAFS),ITP )
CKSK &IDX,NM,IT,JT,MM,NN,A(LAFS),ITP )
      IF (JCONV.NE.2) GO TO 280
C
C     STORE ANGULAR FLUXES FOR ROW J
C
CKSK  CALL STORAF (BT1,A(LQB1),0,J+1,IPVAF+LTVAF*JTP,LTVAF)
      CALL STORAF (BT1,AAA(LQB1),0,J+1,IPVAF+LTVAF*JTP,LTVAF)
CKSK  CALL STORAF (A(LANF),A(LQB1),0,J,IPHAF+LTHAF*JT,LTHAF)
      CALL STORAF (AAA(LANF),AAA(LQB1),0,J,IPHAF+LTHAF*JT,LTHAF)
  280 CONTINUE
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C
C     END OF SWEEPS, BUMP ITERATION COUNT
C
      IITNO=IITNO+1
C
C
C     CALCULATE TOTAL EFFECTIVE ABSORPTION
C
      DO 290 J=1,JT
      DO 290 I=1,IT
      IZ=NA(LDXC+I-1)+NA(LDYC+J-1)
      IC=IABS(IDCS(IZ))
      ID=IDX(I)
      JA=IDYA(J)
  290 AB(ID,JA)=AB(ID,JA)+(CTOT(I,J)-C(IHS,IC)*XDF(I)*YDF(J)*A5(I))*FLUX
     &(1,I,J)*YH(J)
C
C     DETERMINE COARSE MESH REBALANCE FACTORS
C
      IFLAG=1
      IF (IITNO.GT.NLIMIT) GO TO 300
      IF (MOD(IITNO,2).EQ.0) IFLAG=0
  300 CONTINUE
CKSK  CALL REBAL (F,FU,FD,FL,FR,AB,QQ,A(LHA),A(LGA),IM,JM,IP,JP,IBT)
      CALL REBAL (F,FU,FD,FL,FR,AB,QQ,AAA(LHA),AAA(LGA),IM,JM,IP,JP,IBT)
C
C     APPLY REBALANCE FACTORS
C
      DO 310 J=1,JT
      JA=IDYA(J)
      DO 310 I=1,IT
      ID=IDX(I)
      DO 310 N=1,NM
  310 FLUX(N,I,J)=F(ID,JA)*FLUX(N,I,J)
      IF (IBT.EQ.0) GO TO 330
      DO 320 I=1,IT
      ID=IDX(I)
      DO 320 M=1,MM
      BT1(I,M)=BT1(I,M)*F(ID,JM)
  320 BT2(I,M)=BT2(I,M)*F(ID,JM)
  330 IF (IBR.EQ.0) GO TO 350
      DO 340 J=1,JT
      JA=IDYA(J)
      DO 340 M=1,MM
      BR1(J,M)=BR1(J,M)*F(IM,JA)
  340 BR2(J,M)=BR2(J,M)*F(IM,JA)
  350 IF (IBT.NE.3) GO TO 370
      DO 360 M=1,MM
      K3=LBT3-1+(M-1)*IT
      K4=LBT4-1+(M-1)*IT
      DO 360 I=1,IT
      ID=IDX(I)
CKSK  A(K3+I)=A(K3+I)*F(ID,1)
CKSK  A(K4+I)=A(K4+I)*F(ID,1)
      AAA(K3+I)=AAA(K3+I)*F(ID,1)
      AAA(K4+I)=AAA(K4+I)*F(ID,1)
  360 CONTINUE
  370 CONTINUE
C
C     CHECK ERROR
C
      ERR=0.0
      IF (JT.EQ.1) GO TO 400
      IF (IT.EQ.1) GO TO 400
      DO 390 J=2,JT
      DO 390 I=2,IT
      T=FLUX(1,I-1,J-1)+FLUX(1,I-1,J)+FLUX(1,I,J-1)+FLUX(1,I,J)
      IF (T.NE.0.0) GO TO 380
      ERR=ERR+ABS(FLUXA(I-1,J-1)/TS)
      GO TO 390
  380 ERR=AMAX1(ERR,ABS(1.0-(FLUXA(I-1,J-1)+FLUXA(I-1,J)+FLUXA(I,J-1)+FL
     &UXA(I,J))/T))
  390 CONTINUE
      GO TO 430
  400 DO 420 J=1,JT
      DO 420 I=1,IT
      IF (FLUX(1,I,J).NE.0.0) GO TO 410
      ERR=ERR+ABS(FLUXA(I,J)/TS)
      GO TO 420
  410 ERR=AMAX1(ERR,ABS(1.0-FLUXA(I,J)/FLUX(1,I,J)))
  420 CONTINUE
  430 CONTINUE
      IF (IITNO.GT.IITL) GO TO 440
      IF (ERR.GT.EPSI) GO TO 100
      GO TO 490
C
C     REBALANCE FLOWS IF INNER ITERATION UNCONVERGED
C
  440 DO 460 J=1,JM
      FR(1,J)=FR(1,J)*F(1,J)
      DO 450 I=1,IM
      FR(I+1,J)=FR(I+1,J)*F(I,J)
  450 FL(I,J)=FL(I,J)*F(I,J)
  460 FL(IP,J)=FL(IP,J)*F(IM,J)
      DO 480 I=1,IM
      T=F(I,1)
      IF (IBT.EQ.3) T=F(I,JM)
      FU(I,1)=FU(I,1)*T
      DO 470 J=1,JM
      FU(I,J+1)=FU(I,J+1)*F(I,J)
  470 FD(I,J)=FD(I,J)*F(I,J)
      T=F(I,JM)
      IF (IBT.EQ.3) T=F(I,1)
  480 FD(I,JP)=FD(I,JP)*T
  490 IF (JCONV.EQ.0) RETURN
      IF (JCONV.NE.2) GO TO 520
C
C     STORE ANGULAR FLUX ON DISK
C
      IF (IG.EQ.1) CALL REED (NAFLUX,0,0.0,0,4)
C
C     VERTICAL ANGULAR FLUX
C
      L=IPVAF+LTVAF*JTP
      DO 500 J=1,JTP
      CALL REED (0,IPVAF+(J-1)*LTVAF,BT1,LTVAF,1)
CKSK  CALL RITE (NAFLUX,0,A(LBT1),LTVAF,2)
      CALL RITE (NAFLUX,0,AAA(LBT1),LTVAF,2)
      CALL REED (0,L+(J-1)*LTVAF,BT1,LTVAF,1)
CK500 CALL RITE (NAFLUX,0,A(LBT1),LTVAF,2)
  500 CALL RITE (NAFLUX,0,AAA(LBT1),LTVAF,2)
C
C     HORIZONTAL ANGULAR FLUX
C
      L=IPHAF+LTHAF*JT
      DO 510 J=1,JT
CKSK  CALL REED (0,IPHAF+(J-1)*LTHAF,A(LANF),LTHAF,1)
      CALL REED (0,IPHAF+(J-1)*LTHAF,AAA(LANF),LTHAF,1)
CKSK  CALL RITE (NAFLUX,0,A(LANF),LTHAF,2)
      CALL RITE (NAFLUX,0,AAA(LANF),LTHAF,2)
CKSK  CALL REED (0,L+(J-1)*LTHAF,A(LANF),LTHAF,1)
      CALL REED (0,L+(J-1)*LTHAF,AAA(LANF),LTHAF,1)
CK510 CALL RITE (NAFLUX,0,A(LANF),LTHAF,2)
  510 CALL RITE (NAFLUX,0,AAA(LANF),LTHAF,2)
  520 CONTINUE
      IF (I6.NE.0) GO TO 530
C
C     PRINT COARSE MESH BALANCE TABLES
C
      CALL PCMBAL (QQ,FR,FL,FU,FD,AB,IM,JM,IP,JP,IG)
  530 RETURN
      END
