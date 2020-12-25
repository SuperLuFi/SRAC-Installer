      SUBROUTINE WINOUT( FLUX,BR2,BT13,BR23,BT23,P3,P4,WGT,
     1    WMU,WETA,A3,A4,FL,FR,FU,
     2    IDDX,NMWK,IT,JT,MM,NN,AF,AF2,ITP,
     3    ISWEEP,AFLUXG,IAGFLX,IDYA,YH,JM,
     4    IHY,IK,JK,KM,KIM,KJM,NM,IHYDD,BT12,
     5    BR12,BT22,BR22,FCENT,FANG1,FANG2,FEDGI,FEDGJ,AL1D,AL2D,
     6    QT3DDD,QT4DDD,B1D,YHD,CTOTD,COSMUD,A1D,COSETD,A3D,
     7    A2D,A4D,AL1D2,AL2D2,IFLSW1,IFLSW2,DFCENT,LSTA1,
     8    LEND1,LSTA2,LEND2,LSIG,LSTAI1,LSIGI,LSTAI2,
     9    LSTAJ1,LSIGJ,LSTAJ2,WRKT1,WRKT2,WRKD1,WRKD2,
     A    KL,ML,IFIX,JFIX,MFIX,IFLMK,JDLI,IDLI,BTWORK,
     B    IFLG1,IHYMAX,JFLG1,LTEM1,MFCEN)
C
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
      DIMENSION AFLUXG(KM,IK)
C***** TEST DIM ****
      DIMENSION         IHY(JM)
      DIMENSION JDLI(JK),IDLI(IK)
      DIMENSION            IHYDD(KJM)
      DIMENSION BR2 (JK,KM)
      DIMENSION BT12(IK,KM),BR12(JK,KM)
      DIMENSION BT13(IK,KM)
      DIMENSION BT22(IK,KM),BR22(JK,KM)
      DIMENSION BT23(IK,KM),BR23(JK,KM)
      DIMENSION YH(JK)
      DIMENSION WGT(KM),WMU(KM),WETA(KM)
      DIMENSION A3(IK),A4(IK+1)
      DIMENSION FLUX(NM,IK,JK),IDDX(IK),IDYA(JK)
      DIMENSION FL(KIM+1,KJM),              AF(KM,IK+1),AF2(KM,IK+1)
      DIMENSION FR(KIM+1,KJM),FU(KIM,KJM+1)
      DIMENSION P3(NM,KM),P4(NM,KM)
C***** DIM SWEEP WORK ******
      REAL*8 FCENT((IK+KM-1)*JK*KM)
      REAL*8 FANG1((IK+KM-1)*JK*KM),FANG2((IK+KM-1)* JK   *KM)
      REAL*8 FEDGI((IK+KM  )*JK*KM),FEDGJ((IK+KM-1)*(JK+1)*KM)
      REAL*8 AL1D(KM),AL2D(KM)
      REAL*8 QT3DDD(NM,IK,JK,KM)
      REAL*8 QT4DDD(NM,IK,JK,KM)
      REAL*8 B1D(JK),YHD(JK),CTOTD(IK,JK),COSMUD(KM),A1D(IK),COSETD(KM)
      REAL*8 A3D(IK),A2D(IK),A4D(IK+1)
      REAL*8 AL1D2(KM),AL2D2(KM)
      REAL*8 BTWORK(IK,KM)
C***** TEST DIM END ********
      DIMENSION IFLSW1(KM,JK,IK+JK+KM-2)
      DIMENSION IFLSW2(KM,JK,IK+JK+KM-2)
      DIMENSION DFCENT(IK,KM,JK)
      DIMENSION LSTA1(IK+JK+KM-2),LEND1(IK+JK+KM-2)
      DIMENSION LSTA2(IK+JK+KM-2),LEND2(IK+JK+KM-2)
      DIMENSION LSIG (IK+JK+KM-2)
      DIMENSION LSTAI1(IK+JK+KM-1)
      DIMENSION LSIGI(IK+JK+KM-1),LSTAI2(IK+JK+KM-1)
      DIMENSION LSTAJ1(IK+JK+KM-1)
      DIMENSION LSIGJ(IK+JK+KM-1),LSTAJ2(IK+JK+KM-1)
      DIMENSION WRKT1(KM,JK,IK),WRKT2(KM,IK,JK)
      DIMENSION WRKD1(JK*KM,IK),WRKD2(IK*KM,JK)
      DIMENSION KL  (JK*KM),ML  (JK*KM)
C***** ADD NEW ******
      DIMENSION IFIX((JK+1)*KM+100),JFIX ((JK+1)*KM+100)
      DIMENSION MFIX((JK+1)*KM+100),IFLMK((JK+1)*KM+100)
C
C***** ADD EQ *******
      EQUIVALENCE (D(1),NA(1))
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
C
C********** 3 BLOCK START ***************
C
      DO 1113 MLON=1,MFCEN
        FANG1(MLON)=0.0
 1113 CONTINUE
      DO 21650 M=1,MM
        DO 21650 I=1,IT
          K=1
          L=JT+MM-M+I
          FEDGJ(LSIGJ(L)+MM*(K-LSTAJ2(L))+M)=BT12(I,M)
21650 CONTINUE
      DO 21660 M=1,MM
        DO 21660 J=1,JT
          L=IT+JT+MM-M-J+1
          FEDGI(LSIGI(L)+MM*(J-LSTAI2(L))+M)=BR2(J,M)
21660 CONTINUE
      DO 22000 L=LTEM1,1,-1
        LLL=LEND2(L)-LSTA2(L)+1
        IF(L.LE.JT) THEN
            LLL2=LLL-1
            NAD=MM+1
            NAE=0
        ELSE
            LLL2=LLL
            NAD=1
            NAE=-1
        ENDIF
        IF(L.LT.JT+1 ) GO TO 22100
        CALL SWEEP3 (IT,JT,MM,IFLSW2,
     1          FCENT(LSIG (L  )+1),
     2          FANG1(LSIG (L  )+1),
     3          FANG2(LSIG (L  )+1),
     4          FEDGI(LSIGI(L+1)+1),
     5          FEDGI(LSIGI(L  )+1),
     6          FEDGJ(LSIGJ(L+1)+1),
     7          FEDGJ(LSIGJ(L  )+MM+1),
     8          B1D,YHD,CTOTD,COSMUD,A1D,
     9          COSETD,A3D,A2D,A4D,AL1D,AL2D,QT3DDD,
     1          AL1D2,AL2D2,LSTA2(L),
     2          IFIX,JFIX,MFIX,IFLMK,KL,ML,
     3          FANG1(LSIG (L-1)+1),
     4          FANG2(LSIG (L  )+NAD),
     5          L,LLL,LLL2,NAE,NMWK)
                GO TO 22000
22100   IF(L.LE.JT-1) GO TO 22150
        CALL SWEEP3 (IT,JT,MM,IFLSW2,
     1          FCENT(LSIG (L  )+1),
     2          FANG1(LSIG (L  )+1),
     3          FANG2(LSIG (L  )+1),
     4          FEDGI(LSIGI(L+1)+1),
     5          FEDGI(LSIGI(L  )+1),
     6          FEDGJ(LSIGJ(L+1)+1),
     7          FEDGJ(LSIGJ(L  )+1),
     8          B1D,YHD,CTOTD,COSMUD,A1D,
     9          COSETD,A3D,A2D,A4D,AL1D,AL2D,QT3DDD,
     1          AL1D2,AL2D2,LSTA2(L),
     2          IFIX,JFIX,MFIX,IFLMK,KL,ML,
     3          FANG1(LSIG (L-1)+1),
     4          FANG2(LSIG (L  )+NAD),
     5          L,LLL,LLL2,NAE,NMWK)
                GO TO 22000
22150   CALL SWEEP3 (IT,JT,MM,IFLSW2,
     1          FCENT(LSIG (L  )+1),
     2          FANG1(LSIG (L  )+1),
     3          FANG2(LSIG (L  )+1),
     4          FEDGI(LSIGI(L+1)+MM+1),
     5          FEDGI(LSIGI(L  )+1),
     6          FEDGJ(LSIGJ(L+1)+1),
     7          FEDGJ(LSIGJ(L  )+1),
     8          B1D,YHD,CTOTD,COSMUD,A1D,
     9          COSETD,A3D,A2D,A4D,AL1D,AL2D,QT3DDD,
     1          AL1D2,AL2D2,LSTA2(L),
     2          IFIX,JFIX,MFIX,IFLMK,KL,ML,
     3          FANG1(LSIG (L-1)+1),
     4          FANG2(LSIG (L  )+NAD),
     5          L,LLL,LLL2,NAE,NMWK)
22000 CONTINUE
23005 CONTINUE
C
        DO 23000 J=1,JT
        DO 23100 M=1,MM
        DO 23100 I=1,IT
          L=I-J+JT+MM-M
          AFLUXG(M,I)=
     1      FCENT(LSIG(L)+(J-LSTA2(L))*MM+M)
          DFCENT(I,M,J) = AFLUXG(M,I) * WGT(M)
23100   CONTINUE
        IF(JCONV.EQ.0) GO TO 23000
        ISWEEP=3
        WRITE(IAGFLX) IG,J,ISWEEP
        ITDMMD=IT*MM
        CALL RITE(IAGFLX,0,AFLUXG,ITDMMD,2)
23000 CONTINUE
      DO 23140 NMN=1,NMWK
C
        DO 23140 M=1,MM
        DO 23140 J=1,JT
        DO 23140 I=1,IT
          FLUX(NMN,I,J) = FLUX(NMN,I,J) + P3(NMN,M)*DFCENT(I,M,J)
C
23140 CONTINUE
      IC=0
      DO 23170 I=IT,1,-1
        I1=IDDX(I)
        I2=IDDX(I-1)
        IF(I1.EQ.I2) GO TO 23170
        IC=IC+1
C
        DO 23180 M=1,MM
        DO 23180 J=1,JT
          L=I+JT-J+MM-M
          WRKT1(M,J,IC) = FEDGI(LSIGI(L)+(J-LSTAI2(L))*MM+M)
23180   CONTINUE
23170 CONTINUE
C
      IC=0
      DO 3192 IIN=IT,1,-1
        I1=IDDX(IIN)
        I2=IDDX(IIN-1)
        IF(I1.EQ.I2) GO TO 3192
        IC=IC+1
        DO 3202 JJN=1,JFLG1
          IF(JJN.EQ.1) GO TO 3212
          JB=JDLI(JJN-1)
          GO TO 3222
 3212     JB=0
 3222     IF(JJN.EQ.JFLG1) GO TO 3232
          JA=JDLI(JJN)
          GO TO 3242
 3232     JA=JT
 3242     CONTINUE
          DO 3252 IB=1,MM*(JA-JB)
            BC=YH(JB+KL(IB)+1)
            FL(I1,JJN)=
     1         FL(I1,JJN) +
     2         WRKD1(JB*MM+IB,IC) *
     3         WMU(IB-KL(IB)*MM)*BC*A4(IIN)
 3252     CONTINUE
 3202   CONTINUE
 3192 CONTINUE
C
      IC=0
      DO 23175 J=1,JT
        J1=IDYA(J)
        J2=IDYA(J+1)
        IF(J1.EQ.J2) GO TO 23175
        IC=IC+1
C
        DO 23185 M=1,MM
        DO 23185 I=1,IT
          L=I+JT-J+MM-M
          WRKT2(M,I,IC) = FEDGJ(LSIGJ(L)+(J+1-LSTAJ2(L))*MM+M)
23185   CONTINUE
23175 CONTINUE
C
      IC=0
      DO 3197 JJN=1,JT
        J1=IDYA(JJN)
        J2=IDYA(JJN+1)
        IF(J1.EQ.J2) GO TO 3197
        IC=IC+1
        DO 3207 IIN=IFLG1,1,-1
          IF(IIN.EQ.IFLG1) GO TO 3217
          IZ=IDLI(IIN)
          GO TO 3227
 3217     IZ=IT
 3227     IF(IIN.EQ.1) GO TO 3237
          IB=IDLI(IIN-1)
          GO TO 3247
 3237     IB=0
 3247     CONTINUE
          DO 3257 IIB=1,MM*(IZ-IB)
            FU(IIN,J1+1)=
     1         FU(IIN,J1+1) +
     2         WRKD2(IB*MM+IIB,IC) *
     3         WETA(IIB-KL(IIB)*MM)*A3(IB+KL(IIB)+1)
 3257     CONTINUE
 3207   CONTINUE
 3197 CONTINUE
C
      DO 36618 M=1,MM
      DO 36618 I=1,IT
        L=MM+I-M
        K=JT+1
        BT13(I,M)=FEDGJ(LSIGJ(L)+(K-LSTAJ2(L))*MM+M)
36618 CONTINUE
      IF(IBL.EQ.0) GO TO 23168
C
      DO 36615 M=1,MM
      DO 36615 J=1,JT
        L=JT-J+MM-M+1
        BR22(J,M)=FEDGI(LSIGI(L)+(J-LSTAI2(L))*MM+M)
36615 CONTINUE
      DO 26201 M=1,MM
        TEMP = WMU(M) * A4(1)
        DO 26211 JINT = 1,IHYMAX
C
          DO 26211 JC=1,JM
            J=JINT+IHYDD(JC)
            IF(JINT.LE.IHY(JC))
     1      FR(1,JC) = FR(1,JC) + TEMP * BR22(J,M) * YH(J)
C
26211   CONTINUE
26201 CONTINUE
C
      GO TO 25200
C
23168 CONTINUE
      DO 25210 M=1,MM
      DO 25210 J=1,JT
        BR12(J,M)=0.0
25210 CONTINUE
C
25200 CONTINUE
      K =1
      DO 21805 NNN=2,NN+1
C
        DO 21821 I=1,IT
        DO 21821 J=1,JT
          L2=I+J-K+MM-1
          L1=I-J-K+MM+JT
          FANG1(LSIG(L2)+(J-LSTA1(L2))*MM+K) =
     1       FANG2(LSIG(L1)+(J-LSTA2(L1))*MM+K)
21821   CONTINUE
C
        K=K+NNN
21805 CONTINUE
C********* THRU END (SWEEP 3) **********
      IF(JCONV.NE.2) GO TO 24250
      DO 24200 J=1,JT
        DO 24210 M=1,MM
        DO 24210 I=1,ITP
          L=I+JT-J+MM-M
          AF(M,I) = FEDGI(LSIGI(L)+(J-LSTAI2(L))*MM+M)
24210   CONTINUE
24200 CONTINUE
C
C********** 4 BLOCK START ***************
24250 DO 31650 M=1,MM
        DO 31650 I=1,IT
          K=1
          L=MM-M+I
C         FEDGJ(LSIGJ(L)+MM*(K-LSTAJ1(L))+M)=BT22(I,2)   87.10 JAERI
          FEDGJ(LSIGJ(L)+MM*(K-LSTAJ1(L))+M)=BT22(I,M)
31650 CONTINUE
      DO 31660 M=1,MM
        DO 31660 J=1,JT
          L=MM-M+J
          FEDGI(LSIGI(L)+MM*(J-LSTAI1(L))+M)=BR22(J,M)
31660 CONTINUE
C
      DO 32000 L=1,LTEM1
        LLL=LEND1(L)-LSTA1(L)+1
        IF(L.GE.IT+MM-1) THEN
            LLL2=LLL-1
            NAD=MM+1
            NAE=0
        ELSE
            LLL2=LLL
            NAD=1
            NAE=-1
        ENDIF
        IF(L.GT.IT+MM-2) GO TO 32100
        CALL SWEEP4 (IT,JT,MM,IFLSW1,
     1          FCENT(LSIG (L  )+1),
     2          FANG1(LSIG (L  )+1),
     3          FANG2(LSIG (L  )+1),
     4          FEDGI(LSIGI(L  )+1),
     5          FEDGI(LSIGI(L+1)+1),
     6          FEDGJ(LSIGJ(L  )+1),
     7          FEDGJ(LSIGJ(L+1)+MM+1),
     8          B1D,YHD,CTOTD,COSMUD,A1D,
     9          COSETD,A3D,A2D,A4D,AL1D,AL2D,QT4DDD,
     1          AL1D2,AL2D2,LSTA1(L),
     2          IFIX,JFIX,MFIX,IFLMK,KL,ML,
     3          FANG1(LSIG (L+1)+1),
     4          FANG2(LSIG (L  )+NAD),
     5          L,LLL,LLL2,NAE,NMWK)
                GO TO 32000
32100   IF(L.GE.IT+MM  ) GO TO 32150
        CALL SWEEP4 (IT,JT,MM,IFLSW1,
     1          FCENT(LSIG (L  )+1),
     2          FANG1(LSIG (L  )+1),
     3          FANG2(LSIG (L  )+1),
     4          FEDGI(LSIGI(L  )+1),
     5          FEDGI(LSIGI(L+1)+1),
     6          FEDGJ(LSIGJ(L  )+1),
     7          FEDGJ(LSIGJ(L+1)+1),
     8          B1D,YHD,CTOTD,COSMUD,A1D,
     9          COSETD,A3D,A2D,A4D,AL1D,AL2D,QT4DDD,
     1          AL1D2,AL2D2,LSTA1(L),
     2          IFIX,JFIX,MFIX,IFLMK,KL,ML,
     3          FANG1(LSIG (L+1)+1),
     4          FANG2(LSIG (L  )+NAD),
     5          L,LLL,LLL2,NAE,NMWK)
                GO TO 32000
32150   CALL SWEEP4 (IT,JT,MM,IFLSW1,
     1          FCENT(LSIG (L  )+1),
     2          FANG1(LSIG (L  )+1),
     3          FANG2(LSIG (L  )+1),
     4          FEDGI(LSIGI(L  )+MM+1),
     5          FEDGI(LSIGI(L+1)+1),
     6          FEDGJ(LSIGJ(L  )+1),
     7          FEDGJ(LSIGJ(L+1)+1),
     8          B1D,YHD,CTOTD,COSMUD,A1D,
     9          COSETD,A3D,A2D,A4D,AL1D,AL2D,QT4DDD,
     1          AL1D2,AL2D2,LSTA1(L),
     2          IFIX,JFIX,MFIX,IFLMK,KL,ML,
     3          FANG1(LSIG (L+1)+1),
     4          FANG2(LSIG (L  )+NAD),
     5          L,LLL,LLL2,NAE,NMWK)
32000 CONTINUE
33005 CONTINUE
C
        DO 33000 J=1,JT
        DO 33100 M=1,MM
        DO 33100 I=1,IT
          L=I+J-1+MM-M
          AFLUXG(M,I)=
     1      FCENT(LSIG(L)+(J-LSTA1(L))*MM+M)
          DFCENT(I,M,J) = AFLUXG(M,I) * WGT(M)
33100   CONTINUE
        IF(JCONV.EQ.0) GO TO 33000
        ISWEEP=4
        WRITE(IAGFLX) IG,J,ISWEEP
        ITDMMD=IT*MM
        CALL RITE(IAGFLX,0,AFLUXG,ITDMMD,2)
33000 CONTINUE
      DO 33140 NMN=1,NMWK
C
        DO 33140 M=1,MM
        DO 33140 J=1,JT
        DO 33140 I=1,IT
          FLUX(NMN,I,J) = FLUX(NMN,I,J)+  P4(NMN,M)*DFCENT(I,M,J)
C
33140 CONTINUE
      IC=0
      DO 33170 I=1,IT
        I1=IDDX(I)
        I2=IDDX(I+1)
        IF(I1.EQ.I2) GO TO 33170
        IC=IC+1
C
        DO 33180 M=1,MM
        DO 33180 J=1,JT
          L=I+J  +MM-M
          WRKT1(M,J,IC) = FEDGI(LSIGI(L)+(J-LSTAI1(L))*MM+M)
33180   CONTINUE
33170 CONTINUE
C
      IC=0
      DO 7193 IIN=1,IT
        I1=IDDX(IIN)
        I2=IDDX(IIN+1)
        IF(I1.EQ.I2) GO TO 7193
        IC=IC+1
        DO 7203 JJN=1,JFLG1
          IF(JJN.EQ.1) GO TO 7213
          JB=JDLI(JJN-1)
          GO TO 7223
 7213     JB=0
 7223     IF(JJN.EQ.JFLG1) GO TO 7233
          JA=JDLI(JJN)
          GO TO 7243
 7233     JA=JT
 7243     CONTINUE
          DO 7253 IB=1,MM*(JA-JB)
            BC=YH(JB+KL(IB)+1)
            FR(I1+1,JJN)=
     1         FR(I1+1,JJN) +
     2         WRKD1(JB*MM+IB,IC) *
     3         WMU(IB-KL(IB)*MM)*BC*A4(IIN+1)
 7253     CONTINUE
 7203   CONTINUE
 7193 CONTINUE
C
      IC=0
      DO 33175 J=1,JT
        J1=IDYA(J)
        J2=IDYA(J+1)
        IF(J1.EQ.J2) GO TO 33175
        IC=IC+1
C
        DO 33185 M=1,MM
        DO 33185 I=1,IT
          L=I+J  +MM-M
          WRKT2(M,I,IC) = FEDGJ(LSIGJ(L)+(J+1-LSTAJ1(L))*MM+M)
33185  CONTINUE
33175 CONTINUE
C
      IC=0
      DO 3198 JJN=1,JT
        J1=IDYA(JJN)
        J2=IDYA(JJN+1)
        IF(J1.EQ.J2) GO TO 3198
        IC=IC+1
        DO 3208 IIN=1,IFLG1
          IF(IIN.EQ.1) GO TO 3218
          IB=IDLI(IIN-1)
          GO TO 3228
 3218     IB=0
 3228     IF(IIN.EQ.IFLG1) GO TO 3238
          IZ=IDLI(IIN)
          GO TO 3248
 3238     IZ=IT
 3248     CONTINUE
          DO 3258 IIB=1,MM*(IZ-IB)
            FU(IIN,J1+1)=
     1         FU(IIN,J1+1) +
     2         WRKD2(IB*MM+IIB,IC) *
     3         WETA(IIB-KL(IIB)*MM)*A3(IB+KL(IIB)+1)
 3258     CONTINUE
 3208   CONTINUE
 3198 CONTINUE
C
      DO 56618 M=1,MM
      DO 56618 I=1,IT
        L=JT+MM+I-M
        K=JT+1
        BT23(I,M)=FEDGJ(LSIGJ(L)+(K-LSTAJ1(L))*MM+M)
56618 CONTINUE
      DO 56615 M=1,MM
      DO 56615 J=1,JT
        L=IT+MM-M+J
        BR23(J,M)=FEDGI(LSIGI(L)+(J-LSTAI1(L))*MM+M)
56615 CONTINUE
      IF(JCONV.NE.2) GO TO 34250
      DO 34200 J=1,JT
        DO 34218 M=1,MM
        DO 34218 I=1,IT
          L = I+J-1+MM-M
          AF2(M,I+1) = FEDGI(LSIGI(L)+(J-LSTAI1(L))*MM+M)
34218   CONTINUE
CKSK    CALL STORAF (AF     ,A(LQB1),0,J,IPHAF+LTHAF*JT,LTHAF)
        CALL STORAF (AF     ,AAA(LQB1),0,J,IPHAF+LTHAF*JT,LTHAF)
34200 CONTINUE
C
      DO 34205 J=1,JT
        DO 34255 M=1,MM
        DO 34255 I=1,IT
          L = I+J-1+MM-M
          BTWORK(I,M) = FEDGJ(LSIGJ(L)+(J-LSTAJ1(L))*MM+M)
34255   CONTINUE
CKSK    CALL STORAF (BTWORK,A(LQB1),0,J+1,IPVAF+LTVAF*JTP,LTVAF)
        CALL STORAF (BTWORK,AAA(LQB1),0,J+1,IPVAF+LTVAF*JTP,LTVAF)
34205 CONTINUE
34250 CONTINUE
C
      RETURN
      END
