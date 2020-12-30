C             SETBC               LEVEL=1        DATE=81.11.14
      SUBROUTINE SETBC (A,B,P,Q,W,MM,L,IB,IQ,T,C,D,F,ID,AA,II,JJ,KK,A4)
C
      DIMENSION A(L,MM),B(L,MM),P(L,MM),Q(L,MM),W(1),C(L,MM),D(L,MM),
     &F(II,JJ),ID(1),AA(1)
C
      IF (IB.GT.0) GO TO 110
C
C     VACUUM
C
      DO 100 M=1,MM
      DO 100 I=1,L
      A(I,M)=0.0
  100 B(I,M)=0.
      GO TO 220
C
C     CHOICE OF BOUNDARY CONDITIONS
C
  110 GO TO (150,120,160), IB
C
C     WHITE
C
  120 DO 140 I=1,L
      TA=0.
      DO 130 M=1,MM
  130 TA=TA+W(M)*(B(I,M)+A(I,M))
      TA=TA/(T+T)
      DO 140 M=1,MM
      A(I,M)=TA
  140 B(I,M)=TA
      GO TO 180
C
C     REFLECTIVE
C
  150 GO TO 180
C
C     PERIODIC
C
  160 DO 170 I=1,L
      DO 170 M=1,MM
      TA=A(I,M)
      A(I,M)=C(I,M)
      C(I,M)=TA
      TA=B(I,M)
      B(I,M)=D(I,M)
  170 D(I,M)=TA
C
C     COMPUTE PARTIAL FLOW
C
  180 IF (KK.EQ.0) GO TO 200
C
C     TOP OR BOTTOM
C
      DO 190 I=1,L
      K=ID(I)
      DO 190 M=1,MM
  190 F(K,KK)=F(K,KK)+W(M)*AA(I)*(A(I,M)+B(I,M))
      GO TO 220
C
C     RIGHT
C
  200 DO 210 I=1,L
      K=ID(I)
      TA=AA(I)*A4
      DO 210 M=1,MM
  210 F(II,K)=F(II,K)+W(M)*TA*(A(I,M)+B(I,M))
  220 IF (IQ.EQ.0) RETURN
C
C     ADD BOUNDARY FLUX
C
      DO 230 M=1,MM
      DO 230 I=1,L
      A(I,M)=A(I,M)+P(I,M)
  230 B(I,M)=B(I,M)+Q(I,M)
      RETURN
      END
