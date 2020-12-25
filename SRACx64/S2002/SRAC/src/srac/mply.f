C             MPLY                LEVEL=1        DATE=81.11.14
      SUBROUTINE MPLY (X,Y,N)
C
      DIMENSION Y(1)
C
      DO 100 I=1,N
  100 Y(I)=X*Y(I)
      RETURN
      END
