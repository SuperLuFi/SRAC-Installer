      SUBROUTINE OUT ( S,F,BH,BV,AL,P,W,U,E,WU,WE,AL1,AL2,A1,A2,A3,A4,
     1CX,FH,FV,ID,NMD,ITD,JTD,MMD,NND,AF,ITPD,
     2 WK,XQT,XAA,XBB,XCC,XT,KK,KM,XL,II,IID,MMAX,
     3 WK1,WK2,TT,TTI,IDD,IDDD,NEG)
C
C****     APU010    **   ARGUMENTS SENT  --  OUT
C
      COMMON /TW1C/ DDD(1),LIM1,IA(210)
      COMMON /WORK/AAA(1),LIM2,AI(130)
C
      DIMENSION D(212),A(132)
C
      EQUIVALENCE (D(1),DDD(1)),(AAA(1),A(1))
C
      EQUIVALENCE (A(3),BA),(A(4),BC),(A(5),J),(A(6),J1),(A(7),J2)
C
      DIMENSION S(NMD,ITD),F(NMD,ITD),BH(JTD,MMD),BV(ITD,MMD),
     1AL(NND,ITD),P(NMD,MMD),AF(ITPD,MMD)
      DIMENSION W(1),U(1),E(1),WU(1),WE(1),AL1(1),AL2(1),A1(1),A2(1),
     1A3(1),A4(1),CX(1),FH(1),FV(1),ID(1)
C
      EQUIVALENCE (IA(57),MM),(IA(58),NM),(IA(64),IT),( A(8),JCONV),
     1( A(33),ZZ),( A(34),BB),( A(35),CC),( A(36),DD),( A(37),T),
     2( A(38),QT),( A(39),CT),( IA(81),NN),( A(42),AA),( A(43),TI),
     3( A(44),TJ),( A(45),TM)
C
C****     APU011    **   SETTING APU DIM. -- OUT
      DIMENSION  KK(MMAX),KM(MMAX),XL(MMAX),II(IT),IID(IT)
      DIMENSION  WK(IT),XQT(IT),XAA(IT),XBB(IT),XCC(IT),XT(     IT)
      DIMENSION WK1(MMAX,IT),WK2(IT),TT(IT),TTI(IT),IDD(NEG),IDDD(NEG)
C/*
C
C     APU DECLARATION IN OUT
C****     APU012    **   APU  SOURCE
C
C
C
      DO 1140 M7=1,MMAX
      K=KK(M7)
      M=KM(M7)
      DO 1132 I=1,IT
 1132 XQT(I)=0.
      DO 1142 N=1,NM
      DO 1142 I=1,IT
 1142 XQT(I)=XQT(I)+P(N,M)*S(N,I)
      DO 1122 I=1,IT
      XAA(I)=U(M)*A1(I)
      XBB(I)=BA*E(M)*A3(I)
      XCC(I)=A2(I)*(AL1(M)+AL2(M))
      WK2(I)=XBB(I)*BV(I,M)+XCC(I)*AL(K,I)+XQT(I)
      WK(I)=1.0/(XAA(I)+XBB(I)+XCC(I)+CX(I))
 1122 CONTINUE
      DO 1210 I=1,IT
      T     = (XAA(I)*BH(J,M)+WK2(I))*WK(I)
      TM=2.0*T-AL(K,I)
      TI=2.0*T-BH(J,M)
      TJ=2.0*T-BV(I,M)
      IF(TI.LT.0.0) GO TO 110
      IF(TJ.LT.0.0) GO TO 110
      IF(TM.LT.0.0) GO TO 110
      BH(J,M)=TI
      BV(I,M)=TJ
      AL(K,I)=TM
      GO TO 120
  110 ZZ=U(M)*A4(I)
      AA=XAA(I)-ZZ
      BB=0.5*XBB(I)
      DD=A2(I)*AL1(M)
      CC=XCC(I)-DD
      CT=CX(I)
      QT=XQT(I)
      CALL FIXUP(BH(J,M),BV(I,M),AL(K,I))
  120 CONTINUE
      XT(I)=W(M)*T
      WK1(M,I)=BH(J,M)
 1210 CONTINUE
      DO 1150 N=1,NM
      DO 1150 I=1,IT
 1150 F(N,I)=F(N,I)+P(N,M)*XT(I)
 1140 CONTINUE
C/*
C/*
      DO 1300 L=1,NEG
      DO 1213 M=1,MMD
 1213 FH(L)=FH(L)+BC*WU(M)*WK1(M,IDDD(L))*A4(IDDD(L)+1)
 1214 CONTINUE
C/*
      IF(JCONV.NE.2) GO TO 1300
C/*
      DO 1200 M=1,MMD
 1200 AF(I+1,M)=WK1(M,I)
 1300 CONTINUE
C     DO 1300 I=1,IT
C     I1=ID(I)
C     I2=ID(I+1)
C     IF(I2.EQ.I1) GO TO 1214
C     DO 1213 M=1,MMD
C1213 FH(I1+1)=FH(I1+1)+BC*WU(M)*WK1(M,I)*A4(I+1)
C1214 CONTINUE
C/*
C     IF(JCONV.NE.2) GO TO 1300
C/*
C     DO 1200 M=1,MMD
C1200 AF(I+1,M)=WK1(M,I)
C1300 CONTINUE
C/*
C/*
      IF(J2.EQ.J1) RETURN
C
      DO 1195 I=1,IT
      I1=ID(I)
      DO 1190 M=1,MMD
 1190 FV(I1)=FV(I1)+A3(I)*WE(M)*BV(I,M)
 1195 CONTINUE
C
      RETURN
      END
