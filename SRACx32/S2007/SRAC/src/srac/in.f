C             IN                  LEVEL=1        DATE=81.11.14
      SUBROUTINE IN ( S,F,BH,BV,AL,P,W,U,E,WU,WE,AL1,AL2,A1,A2,A3,A4,CX,
     &FH,FV,ID,NMD,ITD,JTD,MMD,NND,AF,ITPD )
C
      COMMON /TW1C/ DDD(1),LIM1,IA(210)
      COMMON /WORK/AAA(1),LIM2
      DIMENSION D(212),A(132)
      EQUIVALENCE (D(1),DDD(1)),(AAA(1),A(1))
C
      EQUIVALENCE (A(3),BA),(A(4),BC),(A(5),J),(A(6),J1),(A(7),J2)
      DIMENSION S(NMD,ITD),F(NMD,ITD),BH(JTD,MMD),BV(ITD,MMD),
     &AL(NND,ITD),P(NMD,MMD),AF(ITPD,MMD)
      DIMENSION W(1),U(1),E(1),WU(1),WE(1),AL1(1),AL2(1),A1(1),A2(1),
     &A3(1),A4(1),CX(1),FH(1),FV(1),ID(1)
C
      EQUIVALENCE (IA(57),MM),(IA(58),NM),(IA(64),IT),(IA(77),ITP),
     &( A(  8),JCONV),( A(33 ),ZZ),( A(34 ),BB),( A(35 ),CC),
     &( A( 36),DD),( A(37 ),T),( A( 38),QT),( A( 39),CT),( D( 83),NN),
     &( A( 42),AA),( A( 43),TI),( A( 44),TJ),( A( 45),TM)
C
      DO 210 IB=1,IT
      I=ITP-IB
      I1=ID(I)
      I2=ID(I-1)
      CT=CX(I)
      M=1
      DO 170 K=1,NN
      DO 160 L=1,K
      QT=0.0
      DO 100 N=1,NM
  100 QT=QT+P(N,M)*S(N,I)
      AA=U(M)*A1(I)
      BB=BA*E(M)*A3(I)
      IF (L.EQ.1) GO TO 110
      CC=A2(I)*(AL1(M)+AL2(M))
      T=(AA*BH(J,M)+BB*BV(I,M)+CC*AL(K,I)+QT)/(AA+BB+CC+CT)
      TM=T+T-AL(K,I)
      GO TO 120
  110 T=(AA*BH(J,M)+BB*BV(I,M)+QT)/(AA+BB+CT)
      TM=T
  120 TI=T+T-BH(J,M)
      TJ=T+T-BV(I,M)
      IF (TI.LT.0.0) GO TO 130
      IF (TJ.LT.0.0) GO TO 130
      IF (TM.LT.0.0) GO TO 130
      BH(J,M)=TI
      BV(I,M)=TJ
      AL(K,I)=TM
      GO TO 140
  130 ZZ=U(M)*A4(I+1)
      AA=AA-ZZ
      BB=0.5*BB
      CC=A2(I)*AL1(M)
      DD=A2(I)*AL2(M)
      CALL FIXUP (BH(J,M),BV(I,M),AL(K,I))
  140 T=W(M)*T
      DO 150 N=1,NM
  150 F(N,I)=F(N,I)+P(N,M)*T
  160 M=M+1
  170 CONTINUE
      IF (I2.EQ.I1) GO TO 190
C
C     COMPUTE HORIZONTAL FLOW
C
      TA=BC*A4(I)
      DO 180 M=1,MM
  180 FH(I1)=FH(I1)+WU(M)*BH(J,M)*TA
  190 CONTINUE
      IF (JCONV.NE.2) GO TO 210
C
C     SAVE HORIZONTAL ANGULAR FLUX
C
      DO 200 M=1,MM
  200 AF(I,M)=BH(J,M)
  210 CONTINUE
      IF (J2.EQ.J1) RETURN
C
C     COMPUTE VERTICAL FLOW
C
      DO 220 I=1,IT
      I1=ID(I)
      DO 220 M=1,MM
  220 FV(I1)=FV(I1)+WE(M)*BV(I,M)*A3(I)
      RETURN
      END
