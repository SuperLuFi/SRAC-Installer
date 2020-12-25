      SUBROUTINE VINOUT(SOTEM,FLUX,BR1,BT1,
     1    BR2,BT2,BR13,BT13,BR23,BT23,
     2    ALFL,P1,P2,P3,P4,WGT,COSMU,
     3    COSETA,WMU,WETA,AL1,AL2,A1,A2,A3,A4,CTOT,FL,FD,FR,FU,
     4    IDDX,NMWK,IT,JT,MM,NN,AF,AF2,ITP,
     5    ISWEEP,AFLUXG,IAGFLX,B1,IDYA,YH,IM,JM,IP,JP,
     6    IHX,IHY, IK,JK,KM,KIM,KJM,NM,
     7    IPLI,PTEM1,PTEM2,PTEM3,PTEM4,IHXDD,IHYDD,BT12,
     8    BR12,BT22,BR22,FCENT,FANG1,FANG2,FEDGI,FEDGJ,AL1D,AL2D,
     9    QT1D,QT1DD,QT1DDD,QT2D,QT2DD,QT2DDD,QT3D,QT3DD,QT3DDD,
     A    QT4D,QT4DD,QT4DDD,B1D,YHD,CTOTD,COSMUD,A1D,COSETD,A3D,
     B    A2D,A4D,AL1D2,AL2D2,IFLSW1,IFLSW2,DFCENT,LONG,LSTA1,
     C    LEND1,LSTA2,LEND2,LSIG,LONGI,LSTAI1,LSIGI,LSTAI2,LONGJ,
     D    LSTAJ1,LSIGJ,LSTAJ2,WRKT1,WRKT2,WRKD1,WRKD2,LAF1,LAF2,
     E    KL,ML,IFIX,JFIX,MFIX,IFLMK,JDLI,IDLI,BTWORK)
C
C---------------------------------------------
C     << NEW VP-VERSION 2 FOR TWOTRAN >>
C            USED  TUNING VIRSION
C                      FEBRUARY , 87''
C---------------------------------------------
C
      COMMON /TW1C/ DDD(1),LIM1,IA(210)
      COMMON /WORK/ AAA(1),LIM2
C
      DIMENSION     D(212),A(132)
C
      EQUIVALENCE  (D(1),DDD(1)),(A(1),AAA(1))
      EQUIVALENCE  (D(130),IPHAF),
     1             (D(131),IPVAF),(A(5),J),(A(6),J1),(A(7),J2),
     2             (D(132),LTHAF),(D(133),LTVAF),
     3             (D(106),LASTEC),(A(52),LAST2)
C
      DIMENSION     NA(1)
C
C
      DIMENSION IPLI (IK*JK*NM)
      DIMENSION PTEM1(IK*JK*NM,KM),PTEM2(IK*JK*NM,KM)
      DIMENSION PTEM3(IK*JK*NM,KM),PTEM4(IK*JK*NM,KM)
      DIMENSION SOTEM(IK*JK*NM)
      DIMENSION AFLUXG(KM,IK)
C***** TEST DIM ****
      DIMENSION IHX(IM),IHY(JM)
      DIMENSION JDLI(JK),IDLI(IK)
      DIMENSION IHXDD(KIM),IHYDD(KJM)
      DIMENSION AL1(KM),AL2(KM)
      DIMENSION BT1 (IK,KM),BR1 (JK,KM)
      DIMENSION BT2 (IK,KM),BR2 (JK,KM)
      DIMENSION BT12(IK,KM),BR12(JK,KM)
      DIMENSION BT13(IK,KM),BR13(JK,KM)
      DIMENSION BT22(IK,KM),BR22(JK,KM)
      DIMENSION BT23(IK,KM),BR23(JK,KM)
      DIMENSION B1(JK),YH(JK),CTOT(IK,JK),COSMU(KM),A1(IK),COSETA(KM)
      DIMENSION WGT(KM),WMU(KM),WETA(KM)
      DIMENSION A3(IK),A2(IK),A4(IK+1)
      DIMENSION FLUX(NM,IK,JK),IDDX(IK),IDYA(JK)
      DIMENSION FL(KIM+1,KJM),FD(KIM,KJM+1),AF(KM,IK+1),AF2(KM,IK+1)
      DIMENSION FR(KIM+1,KJM),FU(KIM,KJM+1)
      DIMENSION P1(NM,KM),P2(NM,KM)
      DIMENSION P3(NM,KM),P4(NM,KM)
C***** DIM SWEEP WORK ******
      REAL*8 FCENT((IK+KM-1)*JK*KM)
      REAL*8 FANG1((IK+KM-1)*JK*KM),FANG2((IK+KM-1)* JK   *KM)
      REAL*8 FEDGI((IK+KM  )*JK*KM),FEDGJ((IK+KM-1)*(JK+1)*KM)
      REAL*8 AL1D(KM),AL2D(KM)
      REAL*8 QT1D(NM*IK*JK,KM),QT1DD(NM,IK*JK,KM),QT1DDD(NM,IK,JK,KM)
      REAL*8 QT2D(NM*IK*JK,KM),QT2DD(NM,IK*JK,KM),QT2DDD(NM,IK,JK,KM)
      REAL*8 QT3D(NM*IK*JK,KM),QT3DD(NM,IK*JK,KM),QT3DDD(NM,IK,JK,KM)
      REAL*8 QT4D(NM*IK*JK,KM),QT4DD(NM,IK*JK,KM),QT4DDD(NM,IK,JK,KM)
      REAL*8 B1D(JK),YHD(JK),CTOTD(IK,JK),COSMUD(KM),A1D(IK),COSETD(KM)
      REAL*8 A3D(IK),A2D(IK),A4D(IK+1)
      REAL*8 AL1D2(KM),AL2D2(KM)
      REAL*8 BTWORK(IK,KM)
C***** TEST DIM END ********
      DIMENSION IFLSW1(KM,JK,IK+JK+KM-2)
      DIMENSION IFLSW2(KM,JK,IK+JK+KM-2)
      DIMENSION DFCENT(IK,KM,JK)
      DIMENSION LONG (IK+JK+KM-2)
      DIMENSION LSTA1(IK+JK+KM-2),LEND1(IK+JK+KM-2)
      DIMENSION LSTA2(IK+JK+KM-2),LEND2(IK+JK+KM-2)
      DIMENSION LSIG (IK+JK+KM-2)
      DIMENSION LONGI(IK+JK+KM-1),LSTAI1(IK+JK+KM-1)
      DIMENSION LSIGI(IK+JK+KM-1),LSTAI2(IK+JK+KM-1)
      DIMENSION LONGJ(IK+JK+KM-1),LSTAJ1(IK+JK+KM-1)
      DIMENSION LSIGJ(IK+JK+KM-1),LSTAJ2(IK+JK+KM-1)
      DIMENSION WRKT1(KM,JK,IK),WRKT2(KM,IK,JK)
      DIMENSION WRKD1(JK*KM,IK),WRKD2(IK*KM,JK)
      DIMENSION LAF1(IK*KM),LAF2(IK*KM)
      DIMENSION KL  (JK*KM),ML  (JK*KM)
C***** ADD NEW ******
      DIMENSION IFIX((JK+1)*KM+100),JFIX ((JK+1)*KM+100)
      DIMENSION MFIX((JK+1)*KM+100),IFLMK((JK+1)*KM+100)
C
C***** ADD EQ *******
CKSK  EQUIVALENCE (D(1),NA(1))
      EQUIVALENCE (DDD(1),NA(1))
      EQUIVALENCE (IA(175),LDXC),(IA(176),LDYC)
C
      EQUIVALENCE (IA(7),IBL),(IA(8),IBR),(IA(9),IBB),(IA(10),IBT),
     1(IA(17),IHS),(IA(21),IQB),(IA(26),IITL),(IA(31),IEDOPT),
     2(IA(33),IQR),(IA(34),IQT),(IA(44),EPSI),(IA(74),ISCP),
     3(IA(78),JTP),(A(68),LQR1),(A(69),LQR2),(A(70),LQT1),
     4(A(71),LQT2),(A(93),LHA),(A(94),LGA),(A(111),LANF),(A(117),LQB1),
     5(A(118),LQB2),(A(8),JCONV),(A(20),IITNO),(A(21),TS),(A(22),IG),
     6(A(33),ZZ),(A(36),DD),(A(37),T),(A(38),QT)
      EQUIVALENCE (A(40),SUMMU),(A(41),SUMETA),
     1(A(43),TI),(A(44),TJ),(A(45),TM),(A(47),ERR),(A(120),LBT3),
     2(A(121),LBT4),(D(84),NLIMIT),(A(49),IFLAG)
C***** ADD DATA **********
CM    DATA ITHRU / 1 /
      COMMON   /CITKXY/ ITHRU
C
C***** PRE 1 THRU START ********
C
      IF(ITHRU.EQ.1) THEN
                     IFLG1  = 0
                     IHYMAX = 0
                     JFLG1  = 0
C
            CALL VININI( P1,P2,P3,P4,COSMU,
     1                   COSETA,AL1,AL2,A1,A2,A3,A4,IDDX,
     2                   NMWK,IT,JT,MM,NN,
     3                   B1,IDYA,YH,      IM,JM,
     4                   IHX,IHY,         IK,JK,KM,KIM,KJM,NM,
     5                   IPLI,PTEM1,PTEM2,PTEM3,PTEM4,
     6                   IHXDD,IHYDD,AL1D,AL2D,
     7                   B1D,YHD,COSMUD,A1D,COSETD,A3D,
     8                   A2D,A4D,AL1D2,AL2D2,
     9                   IFLSW1,IFLSW2,LONG,LSTA1,
     A                   LEND1,LSTA2,LEND2,LSIG,LONGI,
     B                   LSTAI1,LSIGI,LSTAI2,LONGJ,
     C                   LSTAJ1,LSIGJ,LSTAJ2,LAF1,LAF2,
     D                   KL,ML,JDLI,IDLI,
     E                   IFLG1,IHYMAX,JFLG1)
                     ENDIF
C********* PRE 1 THRU END *********
      ITHRU = ITHRU + 1
      MFCEN =(IT+MM-1)*JT*MM
      LTEM1 = IT+JT+MM-2
C********* SET BC *****************
CKSK  CALL SETBC(BT1,BT2,A(LQT1),A(LQT2),WETA,MM,IT,IBT,IQT,SUMETA,
CKSK 1A(LBT3),A(LBT4),FD,IDDX,A3,IM,JP,JP,0 )
      CALL SETBC(BT1,BT2,AAA(LQT1),AAA(LQT2),WETA,MM,IT,IBT,IQT,SUMETA,
     &AAA(LBT3),AAA(LBT4),FD,IDDX,A3,IM,JP,JP,0 )
CKSK  IF (JCONV.EQ.2) CALL STORAF (BT1,A(LQT1),IQT,JTP,IPVAF,LTVAF)
      IF (JCONV.EQ.2) CALL STORAF (BT1,AAA(LQT1),IQT,JTP,IPVAF,LTVAF)
CKSK  CALL SETBC(BR1,BR2,A(LQR1),A(LQR2),WMU,MM,JT,IBR,IQR,SUMMU,
CKSK 1A(LBT3),A(LBT4),FL,IDYA,YH,IP,JM,0,A4(ITP) )
      CALL SETBC(BR1,BR2,AAA(LQR1),AAA(LQR2),WMU,MM,JT,IBR,IQR,SUMMU,
     &AAA(LBT3),AAA(LBT4),FL,IDYA,YH,IP,JM,0,A4(ITP) )
C********* SET BC END *************
C********* PRE INNER IN THRU START *********
      DO 1570 IIN=1,IT
        DO 1570 JJN=1,JT
          CTOTD(IIN,JJN)=CTOT(IIN,JJN)
 1570 CONTINUE
      DO 1580 MMN=1,MM
        DO 1590 IJN=1,IT*JT*NMWK
          QT1D(IJN,MMN)=PTEM1(IJN,MMN)*SOTEM(IJN)
          QT2D(IJN,MMN)=PTEM2(IJN,MMN)*SOTEM(IJN)
          QT3D(IJN,MMN)=PTEM3(IJN,MMN)*SOTEM(IJN)
          QT4D(IJN,MMN)=PTEM4(IJN,MMN)*SOTEM(IJN)
 1590   CONTINUE
 1580 CONTINUE
      IF(NMWK.LT.2) GO TO 1600
      DO 1610 MMN=1,MM
        DO 1620 NNM=2,NMWK
          DO 1630 IJ=1,IT*JT
            QT1DD(1,IJ,MMN)=QT1DD(1,IJ,MMN)+QT1DD(NNM,IJ,MMN)
            QT2DD(1,IJ,MMN)=QT2DD(1,IJ,MMN)+QT2DD(NNM,IJ,MMN)
            QT3DD(1,IJ,MMN)=QT3DD(1,IJ,MMN)+QT3DD(NNM,IJ,MMN)
            QT4DD(1,IJ,MMN)=QT4DD(1,IJ,MMN)+QT4DD(NNM,IJ,MMN)
 1630     CONTINUE
 1620   CONTINUE
 1610 CONTINUE
 1600 DO 1650 M=1,MM
        DO 1650 I=1,IT
          K=JT+1
          L=JT+MM-M+I
          FEDGJ(LSIGJ(L)+MM*(K-LSTAJ1(L))+M)=BT1(I,M)
 1650 CONTINUE
      DO 1660 M=1,MM
        DO 1660 J=1,JT
          L=IT+MM-M+J
          FEDGI(LSIGI(L)+MM*(J-LSTAI1(L))+M)=BR1(J,M)
 1660 CONTINUE
C
C********* NUM 0 THRU END (SWEEP 1) **********
C********* NUM 0-DUSH, NUM 0 THRU START (SWEEP 1) **********
      DO 1112 MLON=1,MFCEN
        FANG1(MLON)=0.0
 1112 CONTINUE
      DO 2000 L=LTEM1,1,-1
        LLL=LEND1(L)-LSTA1(L)+1
        IF(L.LE.JT) THEN
            LLL2=LLL-1
        ELSE
            LLL2=LLL
        ENDIF
        IF(L.LE.IT+MM-1) THEN
            NAD=1
        ELSE
            NAD=MM+1
        ENDIF
        IF(L.LT.IT+MM) GO TO 2100
C
        CALL SWEEP1 (IT,JT,MM,IFLSW1,
     1          FCENT(LSIG (L  )+1),
     2          FANG1(LSIG (L  )+1),
     3          FANG2(LSIG (L  )+1),
     4          FEDGI(LSIGI(L+1)+1),
     5          FEDGI(LSIGI(L  )+MM+1),
     6          FEDGJ(LSIGJ(L+1)+1),
     7          FEDGJ(LSIGJ(L  )+1),
     8          B1D,YHD,CTOTD,COSMUD,A1D,
     9          COSETD,A3D,A2D,A4D,AL1D,AL2D,QT1DDD,
     1          AL1D2,AL2D2,LSTA1(L),
     2          IFIX,JFIX,MFIX,IFLMK,KL,ML,
     3          FANG1(LSIG (L-1)+NAD),
     4          FANG2(LSIG (L  )+1),
     5          L,LLL,LLL2,NMWK)
                GO TO 2000
 2100   IF(L.LE.IT+MM-2) GO TO 2150
        CALL SWEEP1 (IT,JT,MM,IFLSW1,
     1          FCENT(LSIG (L  )+1),
     2          FANG1(LSIG (L  )+1),
     3          FANG2(LSIG (L  )+1),
     4          FEDGI(LSIGI(L+1)+1),
     5          FEDGI(LSIGI(L  )+1),
     6          FEDGJ(LSIGJ(L+1)+1),
     7          FEDGJ(LSIGJ(L  )+1),
     8          B1D,YHD,CTOTD,COSMUD,A1D,
     9          COSETD,A3D,A2D,A4D,AL1D,AL2D,QT1DDD,
     1          AL1D2,AL2D2,LSTA1(L),
     2          IFIX,JFIX,MFIX,IFLMK,KL,ML,
     3          FANG1(LSIG (L-1)+NAD),
     4          FANG2(LSIG (L  )+1),
     5          L,LLL,LLL2,NMWK)
                GO TO 2000
 2150   CALL SWEEP1 (IT,JT,MM,IFLSW1,
     1          FCENT(LSIG (L  )+1),
     2          FANG1(LSIG (L  )+1),
     3          FANG2(LSIG (L  )+1),
     4          FEDGI(LSIGI(L+1)+1),
     5          FEDGI(LSIGI(L  )+1),
     6          FEDGJ(LSIGJ(L+1)+MM+1),
     7          FEDGJ(LSIGJ(L  )+1),
     8          B1D,YHD,CTOTD,COSMUD,A1D,
     9          COSETD,A3D,A2D,A4D,AL1D,AL2D,QT1DDD,
     1          AL1D2,AL2D2,LSTA1(L),
     2          IFIX,JFIX,MFIX,IFLMK,KL,ML,
     3          FANG1(LSIG (L-1)+NAD),
     4          FANG2(LSIG (L  )+1),
     5          L,LLL,LLL2,NMWK)
 2000 CONTINUE
 3005 CONTINUE
C
        DO 3000 J=JT,1,-1
        DO 3100 M=1,MM
        DO 3100 I=1,IT
          L=I+J-1+MM-M
          AFLUXG(M,I)=
     1      FCENT(LSIG(L)+(J-LSTA1(L))*MM+M)
          DFCENT(I,M,J) = AFLUXG(M,I) * WGT(M)
C
 3100   CONTINUE
        IF(JCONV.EQ.0) GO TO 3000
        ISWEEP=1
        WRITE(IAGFLX) IG,J,ISWEEP
        ITDMMD=IT*MM
        CALL RITE(IAGFLX,0,AFLUXG,ITDMMD,2)
 3000 CONTINUE
C
      DO 3140 NMN=1,NMWK
        DO 3140 M=1,MM
        DO 3140 J=1,JT
        DO 3140 I=1,IT
          FLUX(NMN,I,J) = FLUX(NMN,I,J) + P1(NMN,M)*DFCENT(I,M,J)
 3140 CONTINUE
C
      IC=0
      DO 3170 I=IT,1,-1
        I1=IDDX(I)
        I2=IDDX(I-1)
        IF(I1.EQ.I2) GO TO 3170
        IC=IC+1
        DO 3180 M=1,MM
        DO 3180 J=1,JT
          L=I+J-1+MM-M
          WRKT1(M,J,IC) = FEDGI(LSIGI(L)+(J-LSTAI1(L))*MM+M)
 3180   CONTINUE
 3170 CONTINUE
      IC=0
      DO 3190 IIN=IT,1,-1
        I1=IDDX(IIN)
        I2=IDDX(IIN-1)
        IF(I1.EQ.I2) GO TO 3190
          IC=IC+1
          DO 3200 JJN=JFLG1,1,-1
            IF(JJN.EQ.JFLG1) GO TO 3210
              JA=JDLI(JJN)
              GO TO 3220
 3210       JA=JT
 3220       IF(JJN.EQ.1) GO TO 3230
              JB=JDLI(JJN-1)
              GO TO 3240
 3230       JB=0
 3240       CONTINUE
            DO 3250 IB=1,MM*(JA-JB)
              BC=YH(JB+KL(IB)+1)
              FL(I1,JJN)=
     1           FL(I1,JJN) +
     2           WRKD1(JB*MM+IB,IC) *
     3           WMU(IB-KL(IB)*MM)*BC*A4(IIN)
 3250       CONTINUE
 3200   CONTINUE
 3190 CONTINUE
C
      IC=0
      DO 3175 J=JT,1,-1
        J1=IDYA(J)
        J2=IDYA(J-1)
        IF(J1.EQ.J2) GO TO 3175
        IC=IC+1
        DO 3185 M=1,MM
        DO 3185 I=1,IT
          L=I+J-1+MM-M
          WRKT2(M,I,IC) = FEDGJ(LSIGJ(L)+(J-LSTAJ1(L))*MM+M)
 3185   CONTINUE
 3175 CONTINUE
C
      IC=0
      DO 3195 JJN=JT,1,-1
        J1=IDYA(JJN)
        J2=IDYA(JJN-1)
        IF(J1.EQ.J2) GO TO 3195
        IC=IC+1
        DO 3205 IIN=IFLG1,1,-1
          IF(IIN.EQ.IFLG1) GO TO 3215
          IZ=IDLI(IIN)
          GO TO 3225
 3215     IZ=IT
 3225     IF(IIN.EQ.1) GO TO 3235
          IB=IDLI(IIN-1)
          GO TO 3245
 3235     IB=0
 3245 CONTINUE
          DO 3255 IIB=1,MM*(IZ-IB)
            FD(IIN,J1)=
     1         FD(IIN,J1) +
     2         WRKD2(IB*MM+IIB,IC) *
     3         WETA(IIB-KL(IIB)*MM)*A3(IB+KL(IIB)+1)
 3255     CONTINUE
 3205   CONTINUE
 3195 CONTINUE
C
      DO 16618 M=1,MM
      DO 16618 I=1,IT
        L=MM+I-M
        K=1
        BT12(I,M)=FEDGJ(LSIGJ(L)+(K-LSTAJ1(L))*MM+M)
16618 CONTINUE
      IF(IBL.EQ.0) GO TO 3168
C
      DO 16615 M=1,MM
      DO 16615 J=1,JT
        L=MM-M+J
        BR12(J,M)=FEDGI(LSIGI(L)+(J-LSTAI1(L))*MM+M)
16615 CONTINUE
      DO 6201 M=1,MM
        TEMP = WMU(M) * A4(1)
        DO 6211 JINT = 1,IHYMAX
C
          DO 6211 JC=1,JM
            J=JINT+IHYDD(JC)
            IF(JINT.LE.IHY(JC))
     1      FR(1,JC) = FR(1,JC) + TEMP * BR12(J,M) * YH(J)
C
 6211   CONTINUE
 6201 CONTINUE
C
      GO TO 5200
C
 3168 CONTINUE
      DO 5210 M=1,MM
      DO 5210 J=1,JT
        BR12(J,M)=0.0
 5210 CONTINUE
C
 5200 CONTINUE
C
      K =1
      DO 1805 NNN=2,NN+1
C
        DO 1821 I=1,IT
        DO 1821 J=1,JT
          L1=I+J-K+MM-1
          L2=I-J-K+MM+JT
          FANG1(LSIG(L2)+(J-LSTA2(L2))*MM+K) =
     1       FANG2(LSIG(L1)+(J-LSTA1(L1))*MM+K)
C
 1821   CONTINUE
C
        K=K+NNN
 1805 CONTINUE
C
      IF(JCONV.NE.2) GO TO 14120
      DO 14100 J=JT,1,-1
        DO 14100 M=1,MM
        DO 14100 I=1,ITP
          L=I+J-1+MM-M
          AF(M,I) = FEDGI(LSIGI(L)+(J-LSTAI1(L))*MM+M)
14100   CONTINUE
14120 CONTINUE
C********* THRU END (SWEEP 1) **********
C********** 2 BLOCK START ***************
      DO 11650 M=1,MM
        DO 11650 I=1,IT
          K=JT+1
          L=MM-M+I
          FEDGJ(LSIGJ(L)+MM*(K-LSTAJ2(L))+M)=BT2(I,M)
11650 CONTINUE
      DO 11660 M=1,MM
        DO 11660 J=1,JT
          L=JT+MM-M-J+1
          FEDGI(LSIGI(L)+MM*(J-LSTAI2(L))+M)=BR12(J,M)
11660 CONTINUE
C
      DO 12000 L=1,LTEM1
        LLL=LEND2(L)-LSTA2(L)+1
        IF(L.GE.IT+MM-1) THEN
            LLL2=LLL-1
        ELSE
            LLL2=LLL
        ENDIF
        IF(L.LT.JT) THEN
            NAD=MM+1
        ELSE
            NAD=1
        ENDIF
        IF(L.GT.JT-1) GO TO 12100
        CALL SWEEP2 (IT,JT,MM,IFLSW2,
     1          FCENT(LSIG (L  )+1),
     2          FANG1(LSIG (L  )+1),
     3          FANG2(LSIG (L  )+1),
     4          FEDGI(LSIGI(L  )+1),
     5          FEDGI(LSIGI(L+1)+MM+1),
     6          FEDGJ(LSIGJ(L  )+1),
     7          FEDGJ(LSIGJ(L+1)+1),
     8          B1D,YHD,CTOTD,COSMUD,A1D,
     9          COSETD,A3D,A2D,A4D,AL1D,AL2D,QT2DDD,
     1          AL1D2,AL2D2,LSTA2(L),
     2          IFIX,JFIX,MFIX,IFLMK,KL,ML,
     3          FANG1(LSIG (L+1)+NAD),
     4          FANG2(LSIG (L  )+1),
     5          L,LLL,LLL2,NMWK)
                GO TO 12000
12100   IF(L.GE.JT+1   ) GO TO 12150
        CALL SWEEP2 (IT,JT,MM,IFLSW2,
     1          FCENT(LSIG (L  )+1),
     2          FANG1(LSIG (L  )+1),
     3          FANG2(LSIG (L  )+1),
     4          FEDGI(LSIGI(L  )+1),
     5          FEDGI(LSIGI(L+1)+1),
     6          FEDGJ(LSIGJ(L  )+1),
     7          FEDGJ(LSIGJ(L+1)+1),
     8          B1D,YHD,CTOTD,COSMUD,A1D,
     9          COSETD,A3D,A2D,A4D,AL1D,AL2D,QT2DDD,
     1          AL1D2,AL2D2,LSTA2(L),
     2          IFIX,JFIX,MFIX,IFLMK,KL,ML,
     3          FANG1(LSIG (L+1)+NAD),
     4          FANG2(LSIG (L  )+1),
     5          L,LLL,LLL2,NMWK)
                GO TO 12000
12150   CALL SWEEP2 (IT,JT,MM,IFLSW2,
     1          FCENT(LSIG (L  )+1),
     2          FANG1(LSIG (L  )+1),
     3          FANG2(LSIG (L  )+1),
     4          FEDGI(LSIGI(L  )+1),
     5          FEDGI(LSIGI(L+1)+1),
     6          FEDGJ(LSIGJ(L  )+MM+1),
     7          FEDGJ(LSIGJ(L+1)+1),
     8          B1D,YHD,CTOTD,COSMUD,A1D,
     9          COSETD,A3D,A2D,A4D,AL1D,AL2D,QT2DDD,
     1          AL1D2,AL2D2,LSTA2(L),
     2          IFIX,JFIX,MFIX,IFLMK,KL,ML,
     3          FANG1(LSIG (L+1)+NAD),
     4          FANG2(LSIG (L  )+1),
     5          L,LLL,LLL2,NMWK)
12000 CONTINUE
13005 CONTINUE
C
        DO 13000 J=JT,1,-1
        DO 13100 M=1,MM
        DO 13100 I=1,IT
          L=I-J+JT+MM-M
          AFLUXG(M,I)=
     1      FCENT(LSIG(L)+(J-LSTA2(L))*MM+M)
          DFCENT(I,M,J) = AFLUXG(M,I) * WGT(M)
C
13100   CONTINUE
        IF(JCONV.EQ.0) GO TO 13000
        ISWEEP=2
        WRITE(IAGFLX) IG,J,ISWEEP
        ITDMMD=IT*MM
        CALL RITE(IAGFLX,0,AFLUXG,ITDMMD,2)
13000 CONTINUE
C
      DO 13140 NMN=1,NMWK
C
        DO 13140 M=1,MM
        DO 13140 J=1,JT
        DO 13140 I=1,IT
          FLUX(NMN,I,J) = FLUX(NMN,I,J) + P2(NMN,M)*DFCENT(I,M,J)
C
13140 CONTINUE
      IC=0
      DO 13170 I=1,IT
        I1=IDDX(I)
        I2=IDDX(I+1)
        IF(I1.EQ.I2) GO TO 13170
        IC=IC+1
C
        DO 13180 M=1,MM
        DO 13180 J=1,JT
          L=I+JT-J+MM-M  +1
          WRKT1(M,J,IC) = FEDGI(LSIGI(L)+(J-LSTAI2(L))*MM+M)
13180   CONTINUE
13170 CONTINUE
C
      IC=0
      DO 5191 IIN=1,IT
        I1=IDDX(IIN)
        I2=IDDX(IIN+1)
        IF(I1.EQ.I2) GO TO 5191
        IC=IC+1
        DO 5201 JJN=JFLG1,1,-1
          IF(JJN.EQ.JFLG1) GO TO 5211
          JA=JDLI(JJN)
          GO TO 5221
 5211     JA=JT
 5221     IF(JJN.EQ.1) GO TO 5231
          JB=JDLI(JJN-1)
          GO TO 5241
 5231     JB=0
 5241     CONTINUE
          DO 5251 IB=1,MM*(JA-JB)
            BC=YH(JB+KL(IB)+1)
            FR(I1+1,JJN)=
     1         FR(I1+1,JJN) +
     2         WRKD1(JB*MM+IB,IC) *
     3         WMU(IB-KL(IB)*MM)*BC*A4(IIN+1)
 5251     CONTINUE
 5201   CONTINUE
 5191 CONTINUE
      IC=0
      DO 13175 J=JT,1,-1
        J1=IDYA(J)
        J2=IDYA(J-1)
        IF(J1.EQ.J2) GO TO 13175
        IC=IC+1
C
        DO 13185 M=1,MM
        DO 13185 I=1,IT
          L=I+JT-J+MM-M +1
          WRKT2(M,I,IC) = FEDGJ(LSIGJ(L)+(J-LSTAJ2(L))*MM+M)
13185  CONTINUE
13175 CONTINUE
      IC=0
      DO 3196 JJN=JT,1,-1
        J1=IDYA(JJN)
        J2=IDYA(JJN-1)
        IF(J1.EQ.J2) GO TO 3196
        IC=IC+1
        DO 3206 IIN=1,IFLG1
          IF(IIN.EQ.1) GO TO 3216
          IB=IDLI(IIN-1)
          GO TO 3226
 3216     IB=0
 3226     IF(IIN.EQ.IFLG1) GO TO 3236
          IZ=IDLI(IIN)
          GO TO 3246
 3236     IZ=IT
 3246     CONTINUE
          DO 3256 IIB=1,MM*(IZ-IB)
            FD(IIN,J1)=
     1         FD(IIN,J1) +
     2         WRKD2(IB*MM+IIB,IC) *
     3         WETA(IIB-KL(IIB)*MM)*A3(IB+KL(IIB)+1)
 3256     CONTINUE
 3206   CONTINUE
 3196 CONTINUE
C
      DO 26618 M=1,MM
      DO 26618 I=1,IT
        L=JT+MM+I-M
        K=1
        BT22(I,M)=FEDGJ(LSIGJ(L)+(K-LSTAJ2(L))*MM+M)
26618 CONTINUE
      DO 26615 M=1,MM
      DO 26615 J=1,JT
        L=IT+JT+MM-M-J+1
        BR13(J,M)=FEDGI(LSIGI(L)+(J-LSTAI2(L))*MM+M)
26615 CONTINUE
C********* THRU END ( SWEEP 2 ) *********
      IF(JCONV.NE.2) GO TO 14150
      DO 14125 J=JT,1,-1
        DO 14115 M=1,MM
        DO 14115 I=1,ITP
          L=I+JT-J+MM-M
          AF2(M,I+1) = FEDGI(LSIGI(L)+(J-LSTAI2(L))*MM+M)
14115   CONTINUE
CKSK    CALL STORAF (AF     ,A(LQT1),0,J,IPHAF,LTHAF)
        CALL STORAF (AF     ,AAA(LQT1),0,J,IPHAF,LTHAF)
14125 CONTINUE
C
      DO 14118 J=JT,1,-1
        DO 14116 M=1,MM
        DO 14116 I=1,IT
          L=I+J-1+MM-M
          BTWORK(I,M) = FEDGJ(LSIGJ(L)+(J-LSTAJ1(L))*MM+M)
14116   CONTINUE
CKSK    CALL STORAF (BTWORK ,A(LQT1),0,J,IPVAF,LTVAF)
        CALL STORAF (BTWORK ,AAA(LQT1),0,J,IPVAF,LTVAF)
14118 CONTINUE
C
CKSK0 CALL SETBC(BT12,BT22,A(LQB1),A(LQB2),WETA,MM,IT,IBB,IQB,SUMETA,
CKSK 1A(LBT3),A(LBT4),FU,IDDX,A3,IM,JP,1,0 )
14150 CALL SETBC(BT12,BT22,AAA(LQB1),AAA(LQB2),WETA,MM,IT,IBB,IQB,
     &           SUMETA,AAA(LBT3),AAA(LBT4),FU,IDDX,A3,IM,JP,1,0 ) 
      IF(JCONV.EQ.2)
     &         CALL STORAF (BT12,AAA(LQB1),IQB,1,IPVAF+LTVAF*JTP,LTVAF)
CKSK &         CALL STORAF (BT12,A(LQB1),IQB,1,IPVAF+LTVAF*JTP,LTVAF)
C
C   *** CALL WINOUT FOR SWEEP3 & SWEEP4
C
       CALL  WINOUT( FLUX,BR2,BT13,BR23,BT23,P3,P4,WGT,
     1               WMU,WETA,A3,A4,FL,FR,FU,
     2               IDDX,NMWK,IT,JT,MM,NN,AF,AF2,ITP,
     3               ISWEEP,AFLUXG,IAGFLX,IDYA,YH,JM,
     4               IHY,IK,JK,KM,KIM,KJM,NM,IHYDD,BT12,
     5    BR12,BT22,BR22,FCENT,FANG1,FANG2,FEDGI,FEDGJ,AL1D,AL2D,
     6    QT3DDD,QT4DDD,B1D,YHD,CTOTD,COSMUD,A1D,COSETD,A3D,
     7    A2D,A4D,AL1D2,AL2D2,IFLSW1,IFLSW2,DFCENT,LSTA1,
     8    LEND1,LSTA2,LEND2,LSIG,LSTAI1,LSIGI,LSTAI2,
     9    LSTAJ1,LSIGJ,LSTAJ2,WRKT1,WRKT2,WRKD1,WRKD2,
     A    KL,ML,IFIX,JFIX,MFIX,IFLMK,JDLI,IDLI,BTWORK,
     B    IFLG1,IHYMAX,JFLG1,LTEM1,MFCEN)
C
C
C
      RETURN
      END