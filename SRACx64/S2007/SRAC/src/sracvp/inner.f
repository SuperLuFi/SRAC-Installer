      SUBROUTINE INNER ( C,Q,FLUX,BR1,BR2,BT1,BT2,FLUXA,COSMU,COSETA,
     1AL1,AL2,ALFL,B1,F,AB,FU,FD,FR,FL,QQ,WETA,WMU,WGT,P1,P2,P3,P4,A1,
     2A2,A3,A4,A5,IDX,IDY,IDYA,IDCS,NN,YH,SOURCE,CTOT,XDF,YDF,IHM,MT,NM,
     3IT,JT,MM,IM,JM,IP,JP )
C
C---------------------------------------------
C     <<     SUBROUTINE  INNER     >>
C      USED  TUNING VIRSION
C                     FEBRUARY , 87''
C---------------------------------------------
C
      COMMON /TW1C/ DDD(1),LIM1,IA(210)
      COMMON /WORK/ AAA(1),LIM2
      COMMON /TWVPWK/ IANGAD
C
      DIMENSION D(212),A(132)
C
      EQUIVALENCE (D(1),DDD(1)),(A(1),AAA(1))
      EQUIVALENCE (A(3),BA),(A(4),BC),(D(108),IPSO),(D(130),IPHAF),
     1(D(131),IPVAF),(A(5),J),(D(128),I6),(A(6),J1),(A(7),J2),
     2(D(132),LTHAF),(D(109),LTSO),(D(133),LTVAF),(D(158),NAFLUX),
     3(D(106),LASTEC),(A(52),LAST2),(D(171),LIHX),(D(172),LIHY)
C
C
      DIMENSION C(IHM,MT),Q(NM,IT,JT),FLUX(NM,IT,JT),FLUXA(IT,JT),
     1BR1(JT,MM),BR2(JT,MM),COSMU(1),COSETA(1),BT1(IT,MM),AL1(1),AL2(1),
     2BT2(IT,MM),ALFL(NN,IT),B1(1),F(IM,JM),AB(IM,JM),QQ(IM,JM),
     3FU(IM,JP),FD(IM,JP),FR(IP,JM),FL(IP,JM),WETA(1),WMU(1),WGT(1),
     4P1(NM,MM),P2(NM,MM),P3(NM,MM),P4(NM,MM),A1(1),A2(1),A3(1),A4(1),
     5A5(1),IDCS(1),IDX(1),IDY(1),IDYA(1),YH(1),SOURCE(NM,IT,JT)
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
     1(IA(17),IHS),(IA(21),IQB),(IA(26),IITL),(IA(31),IEDOPT),
     2(IA(33),IQR),(IA(34),IQT),(IA(44),EPSI),(IA(74),ISCP),
     3(IA(77),ITP),(IA(78),JTP),(A(68),LQR1),(A(69),LQR2),(A(70),LQT1),
     4(A(71),LQT2),(A(93),LHA),(A(94),LGA),(A(111),LANF),(A(117),LQB1),
     5(A(118),LQB2),(A(8),JCONV),(A(20),IITNO),(A(21),TS),(A(22),IG),
     6(A(33),ZZ),(A(34),BB),(A(35),CC),(A(36),DD),(A(37),T),(A(38),QT),
     7(A(39),CT)
      EQUIVALENCE (A(74),LBT1),(A(40),SUMMU),(A(41),SUMETA),(A(42),AA),
     1(A(43),TI),(A(44),TJ),(A(45),TM),(A(47),ERR),(A(120),LBT3),
     2(A(121),LBT4),(D(84),NLIMIT),(A(49),IFLAG)
C
C     RESTORE SOURCE TO GROUP / STORE OLD FLUX / ZERO NEW FLUX /
C
  100 CONTINUE
C      VP WORK REGION DEFINE
      IK = IT
      JK = JT
      KM = MM
      N01 = LAST2 + LASTEC + 1
      N02 = N01 + IK * JK * NM
      N03 = N02 + IK * JK * KM * NM
      N04 = N03 + IK * JK * KM * NM
      N05 = N04 + IK * JK * KM * NM
      N07 = N05 + IK * JK * KM * NM
      N08 = N07 + IM
      N09 = N08 + JM
      N10 = N09 + IK * KM
      N11 = N10 + JK * KM
      N12 = N11 + IK * KM
      N13 = N12 + JK * KM
      N14 = N13 + (IK + JK + KM - 2) * JK * KM
      N15 = N14 + (IK + JK + KM - 2) * JK * KM
      N16 = N15 + IK * JK * KM
      N17 = N16 + IK + JK + KM - 2
      N18 = N17 + IK + JK + KM - 2
      N19 = N18 + IK + JK + KM - 2
      N20 = N19 + IK + JK + KM - 2
      N21 = N20 + IK + JK + KM - 2
      N22 = N21 + IK + JK + KM - 2
      N23 = N22 + IK + JK + KM - 1
      N24 = N23 + IK + JK + KM - 1
      N25 = N24 + IK + JK + KM - 1
      N26 = N25 + IK + JK + KM - 1
      N27 = N26 + IK + JK + KM - 1
      N28 = N27 + IK + JK + KM - 1
      N29 = N28 + IK + JK + KM - 1
      N30 = N29 + IK + JK + KM - 1
      N31 = N30 + IK * JK * KM
      N32 = N31 + IK * JK * KM
      N33 = N32 + IK * KM
      N34 = N33 + IK * KM
      N35 = N34 + JK * KM
      N36 = N35 + JK * KM
      N37 = N36 + (JK + 1) * KM + 100
      N38 = N37 + (JK + 1) * KM + 100
      N39 = N38 + (JK + 1) * KM + 100
      N71 = N39 + (JK + 1) * KM + 100
      N72 = N71 + JK
      N40 = N72 + IK
      IF (MOD(N40,2) .EQ. 0) N40 = N40 +  1
      N41 = N40 + IK * KM * 2
      N42 = N41 + (IK + KM - 1) * JK * KM * 2
      N43 = N42 + (IK + KM - 1) * JK * KM * 2
      N44 = N43 + (IK + KM - 1) * JK * KM * 2
      N45 = N44 + (IK + KM ) * JK * KM * 2
      N46 = N45 + (IK + KM - 1) * (JK + 1) * KM * 2
      N47 = N46 + KM * 2
      N48 = N47 + KM * 2
      N49 = N48 + NM * IK * JK * KM * 2
      N50 = N49 + NM * IK * JK * KM * 2
      N51 = N50 + NM * IK * JK * KM * 2
      N52 = N51 + NM * IK * JK * KM * 2
      N61 = N52 + JK * 2
      N62 = N61 + JK * 2
      N63 = N62 + IK * JK * 2
      N64 = N63 + KM * 2
      N65 = N64 + IK * 2
      N66 = N65 + KM * 2
      N67 = N66 + IK * 2
      N68 = N67 + IK * 2
      N69 = N68 + (IK + 1) * 2
      N70 = N69 + KM * 2
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
  110     K=1
          DO 120 N=1,ISCP
            T=(2*N-1)*C(IHS,N-1-IC)*T1
            DO 120 L=1,N
              SOURCE(K,I,J)=(Q(K,I,J)+T*FLUX(K,I,J))*A5(I)
              K=K+1
  120     CONTINUE
  130     DO 140 N=1,NM
            FLUX(N,I,J)=0.0
  140     CONTINUE
  150 CONTINUE
C
C     PREPARE REBALANCE ARRAYS
C
      DO 170 J=1,JM
        DO 160 I=1,IM
          F(I,J)=1.0
          AB(I,J)=0.0
  160   CONTINUE
        DO 170 I=1,IP
          FR(I,J)=0.0
          FL(I,J)=0.0
  170 CONTINUE
      DO 180 J=1,JP
        DO 180 I=1,IM
          FU(I,J)=0.
          FD(I,J)=0.
  180 CONTINUE
      LAFS=LANF+ITP*MM
      LANGAD = LAST2+IANGAD
      ICON7  = 1
C ******** CALL VP MODULE START ********
      CALL VINOUT (SOURCE,FLUX,BR1,BT1,BR2,BT2,BR1,BT1,BR2,BT2,
     1    ALFL,P1,P2,P3,P4,WGT,
     2    COSMU,COSETA,WMU,WETA,AL1,AL2,A1,A2,A3,A4,CTOT,FL,FD,FR,FU,
     3    IDX, NM, IT, JT, MM, NN, AAA(LANF), AAA(LAFS) , ITP ,
     4    ISWEEP, AAA(LANGAD), ICON7 , B1 , IDYA , YH,IM,JM,IP,JP,
     5    DDD(LIHX) , DDD(LIHY) ,IT,JT,MM,IM,JM,NM,
     6    AAA(N01),AAA(N02),AAA(N03),AAA(N04),AAA(N05),
     7    AAA(N07),AAA(N08),AAA(N09),AAA(N10),
     8    AAA(N11),AAA(N12),AAA(N41),AAA(N42),AAA(N43),
     9    AAA(N44),AAA(N45),AAA(N46),AAA(N47),AAA(N48),
     A    AAA(N48),AAA(N48),AAA(N49),AAA(N49),AAA(N49),
     B    AAA(N50),AAA(N50),AAA(N50),AAA(N51),AAA(N51),
     C    AAA(N51),AAA(N52),AAA(N61),AAA(N62),AAA(N63),
     D    AAA(N64),AAA(N65),AAA(N66),AAA(N67),AAA(N68),
     E    AAA(N69),AAA(N70),AAA(N13),AAA(N14),AAA(N15),
     F    AAA(N16),AAA(N17),AAA(N18),AAA(N19),AAA(N20),
     G    AAA(N21),AAA(N22),AAA(N23),AAA(N24),AAA(N25),AAA(N26),
     H    AAA(N27),AAA(N28),AAA(N29),AAA(N30),AAA(N31),AAA(N30),
     I    AAA(N31),AAA(N32),AAA(N33),AAA(N34),AAA(N35),AAA(N36),
     J    AAA(N37),AAA(N38),AAA(N39),AAA(N71),AAA(N72),AAA(N40))
C
      IITNO=IITNO+1
C
C     CALCULATE TOTAL EFFECTIVE ABSORPTION
C
      DO 290 J=1,JT
        DO 290 I=1,IT
          IZ=NA(LDXC+I-1)+NA(LDYC+J-1)
          IC=IABS(IDCS(IZ))
          ID=IDX(I)
          JA=IDYA(J)
          AB(ID,JA)=AB(ID,JA)+(CTOT(I,J)-C(IHS,IC)*XDF(I)*YDF(J)*A5(I))
     1             *FLUX(1,I,J)*YH(J)
  290 CONTINUE
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
            FLUX(N,I,J)=F(ID,JA)*FLUX(N,I,J)
  310 CONTINUE
      IF (IBT.EQ.0) GO TO 330
      DO 320 I=1,IT
        ID=IDX(I)
        DO 320 M=1,MM
          BT1(I,M)=BT1(I,M)*F(ID,JM)
          BT2(I,M)=BT2(I,M)*F(ID,JM)
  320 CONTINUE
  330 IF (IBR.EQ.0) GO TO 350
      DO 340 J=1,JT
        JA=IDYA(J)
        DO 340 M=1,MM
          BR1(J,M)=BR1(J,M)*F(IM,JA)
          BR2(J,M)=BR2(J,M)*F(IM,JA)
  340 CONTINUE
  350 IF (IBT.NE.3) GO TO 370
      DO 360 M=1,MM
        K3=LBT3-1+(M-1)*IT
        K4=LBT4-1+(M-1)*IT
        DO 360 I=1,IT
          ID=IDX(I)
CKSK      A(K3+I)=A(K3+I)*F(ID,1)
CKSK      A(K4+I)=A(K4+I)*F(ID,1)
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
  380     ERR=AMAX1(ERR,ABS(1.0-(FLUXA(I-1,J-1)+FLUXA(I-1,J)
     1                          +FLUXA(I  ,J-1)+FLUXA(I  ,J))/T))
  390 CONTINUE
      GO TO 430
  400 DO 420 J=1,JT
        DO 420 I=1,IT
          IF (FLUX(1,I,J).NE.0.0) GO TO 410
          ERR=ERR+ABS(FLUXA(I,J)/TS)
          GO TO 420
  410     ERR=AMAX1(ERR,ABS(1.0-FLUXA(I,J)/FLUX(1,I,J)))
  420 CONTINUE
  430 CONTINUE
C
      IF( JCONV .GT. 0 )  GO  TO  490
C
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
          FL(I,J)=FL(I,J)*F(I,J)
  450   CONTINUE
        FL(IP,J)=FL(IP,J)*F(IM,J)
  460 CONTINUE
      DO 480 I=1,IM
        T=F(I,1)
        IF (IBT.EQ.3) T=F(I,JM)
        FU(I,1)=FU(I,1)*T
        DO 470 J=1,JM
          FU(I,J+1)=FU(I,J+1)*F(I,J)
          FD(I,J)=FD(I,J)*F(I,J)
  470   CONTINUE
        T=F(I,JM)
        IF (IBT.EQ.3) T=F(I,1)
        FD(I,JP)=FD(I,JP)*T
  480 CONTINUE
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
CKSK    CALL RITE (NAFLUX,0,A(LBT1),LTVAF,2)
        CALL RITE (NAFLUX,0,AAA(LBT1),LTVAF,2)
        CALL REED (0,L+(J-1)*LTVAF,BT1,LTVAF,1)
CKSK    CALL RITE (NAFLUX,0,A(LBT1),LTVAF,2)
        CALL RITE (NAFLUX,0,AAA(LBT1),LTVAF,2)
  500 CONTINUE
C
C     HORIZONTAL ANGULAR FLUX
C
      L=IPHAF+LTHAF*JT
      DO 510 J=1,JT
CKSK    CALL REED (0,IPHAF+(J-1)*LTHAF,A(LANF),LTHAF,1)
CKSK    CALL RITE (NAFLUX,0,A(LANF),LTHAF,2)
CKSK    CALL REED (0,L+(J-1)*LTHAF,A(LANF),LTHAF,1)
CKSK    CALL RITE (NAFLUX,0,A(LANF),LTHAF,2)
        CALL REED (0,IPHAF+(J-1)*LTHAF,AAA(LANF),LTHAF,1)
        CALL RITE (NAFLUX,0,AAA(LANF),LTHAF,2)
        CALL REED (0,L+(J-1)*LTHAF,AAA(LANF),LTHAF,1)
        CALL RITE (NAFLUX,0,AAA(LANF),LTHAF,2)
  510 CONTINUE
  520 CONTINUE
      IF (I6.NE.0) GO TO 530
C
C     PRINT COARSE MESH BALANCE TABLES
C
      CALL PCMBAL (QQ,FR,FL,FU,FD,AB,IM,JM,IP,JP,IG)
  530 RETURN
      END
