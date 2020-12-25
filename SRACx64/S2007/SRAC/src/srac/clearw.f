C             CLEARW              LEVEL=1        DATE=81.11.14
      SUBROUTINE CLEARW (X,Y,N)
C
      DIMENSION Y(1)
C
      DO 100 I=1,N
  100 Y(I)=X
      RETURN
      END
