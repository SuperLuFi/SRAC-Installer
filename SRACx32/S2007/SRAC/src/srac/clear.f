C             CLEAR               LEVEL=1        DATE=80.09.29
      SUBROUTINE CLEAR(X,Y,N)
C
      DIMENSION Y(1)
C
      DO 100 I=1,N
  100 Y(I)=X
      RETURN
      END
