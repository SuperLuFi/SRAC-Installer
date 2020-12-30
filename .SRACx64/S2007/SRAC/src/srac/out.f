C             OUT                 LEVEL=1        DATE=81.11.14
      SUBROUTINE OUT ( S,F,BH,BV,AL,P,W,U,E,WU,WE,AL1,AL2,A1,A2,A3,A4,
     &CX,FH,FV,ID,NMD,ITD,JTD,MMD,NND,AF,ITPD )
C
      COMMON /TW1C/ DDD(1),LIM1,IA(210)
      COMMON /WORK/AAA(1),LIM2
      DIMENSION D(212),A(132)
      EQUIVALENCE (D(1),DDD(1)),(AAA(1),A(1))
      EQUIVALENCE (A(3),BA),(A(4),BC),(A(5),J),(A(6),J1),(A(7),J2)
      DIMENSION S(NMD,ITD),F(NMD,ITD),BH(JTD,MMD),BV(ITD,MMD),
     &AL(NND,ITD),P(NMD,MMD),AF(ITPD,MMD)
      DIMENSION W(1),U(1),E(1),WU(1),WE(1),AL1(1),AL2(1),A1(1),A2(1),
     &A3(1),A4(1),CX(1),FH(1),FV(1),ID(1)
C
      EQUIVALENCE (IA(57),MM),(IA(58),NM),(IA(64),IT),( A(8),JCONV),
     &( A(33),ZZ),( A(34),BB),( A(35),CC),( A(36),DD),( A(37),T),
     &( A(38),QT),( A(39),CT),( IA(81),NN),( A(42),AA),( A(43),TI),
     &( A(44),TJ),( A(45),TM)
C
      DO 180 I=1,IT
      I1=ID(I)
      I2=ID(I+1)
      CT=CX(I)
      DO 140 K=1,NN
      KA=(K*(K+1))/2+1
      DO 140 L=1,K
      M=KA-L
      QT=0.0
      DO 100 N=1,NM
  100 QT=QT+P(N,M)*S(N,I)
      AA=U(M)*A1(I)
      BB=BA*E(M)*A3(I)
      CC=A2(I)*(AL1(M)+AL2(M))
      T=(AA*BH(J,M)+BB*BV(I,M)+CC*AL(K,I)+QT)/(AA+BB+CC+CT)
      TM=T+T-AL(K,I)
      TI=T+T-BH(J,M)
      TJ=T+T-BV(I,M)
      IF (TI.LT.0.0) GO TO 110
      IF (TJ.LT.0.0) GO TO 110
      IF (TM.LT.0.0) GO TO 110
      BH(J,M)=TI
      BV(I,M)=TJ
      AL(K,I)=TM
      GO TO 120
  110 ZZ=U(M)*A4(I)
      AA=AA-ZZ
      BB=0.5*BB
      DD=A2(I)*AL1(M)
      CC=CC-DD
      CALL FIXUP (BH(J,M),BV(I,M),AL(K,I))
  120 T=W(M)*T
      DO 130 N=1,NM
  130 F(N,I)=F(N,I)+P(N,M)*T
  140 CONTINUE
      IF (I2.EQ.I1) GO TO 160
C
C     COMPUTE HORIZONTAL FLOW
C
      TA=BC*A4(I+1)
      DO 150 M=1,MM
  150 FH(I1+1)=FH(I1+1)+WU(M)*BH(J,M)*TA
  160 CONTINUE
      IF (JCONV.NE.2) GO TO 180
C
C     SAVE HORIZONTAL ANGULAR FLUX
C
      DO 170 M=1,MM
  170 AF(I+1,M)=BH(J,M)
  180 CONTINUE
      IF (J2.EQ.J1) RETURN
C
C     COMPUTE VERTICAL FLOW
C
      DO 190 I=1,IT
      I1=ID(I)
      DO 190 M=1,MM
  190 FV(I1)=FV(I1)+WE(M)*BV(I,M)*A3(I)
      RETURN
      END
