C             CLEA                LEVEL=1        DATE=80.04.12
      SUBROUTINE   CLEA(N, A, B)
C
      DIMENSION    IA(1),A(1)
C
      DO 10  I=1,N
      A(I)=B
   10 CONTINUE
      RETURN
C
      ENTRY       ICLEA(N,IA,IB)
C
      DO 20 I=1,N
      IA(I)=IB
   20 CONTINUE
      RETURN
C
      ENTRY        SET(A,N,B1,B2)
C
      DO 30 I=1,N
      IF(A(I).NE.B1) GO TO 30
      A(I)=B2
   30 CONTINUE
      RETURN
      END
