      SUBROUTINE CLEA( A , LENG , B )
C***********************************************************************
C     CLEA  IS USED FOR VALUE SET OF REAL    1-DIMENSION ARRAY.
C***********************************************************************
      DIMENSION    A(1)
C
      DO 100 I = 1 , LENG
      A(I) = B
  100 CONTINUE
      RETURN
      END
