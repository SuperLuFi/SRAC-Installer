      SUBROUTINE MOVEVT(A,B,N)
      DIMENSION A(1),B(1),C(1)
      DO 10 I=1,N
      B(I)=A(I)
   10 CONTINUE
      RETURN
C
      ENTRY ADDVT(A,B,C,N)
      DO 20 I=1,N
      C(I)=A(I)+B(I)
   20 CONTINUE
      RETURN
C
      ENTRY SUBVT(A,B,C,N)
      DO 30 I=1,N
      C(I)=A(I)-B(I)
   30 CONTINUE
      RETURN
C
      ENTRY MULTVT(A,B,C,N)
      DO 40 I=1,N
      C(I)=A(I)*B(I)
   40 CONTINUE
      RETURN
C
      ENTRY DIVVT(A,B,C,N)
      DO 50 I=1,N
      C(I)=0.
      IF(B(I).NE.0.) C(I)=A(I)/B(I)
   50 CONTINUE
      RETURN
      END
