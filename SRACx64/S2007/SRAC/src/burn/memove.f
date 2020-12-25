      SUBROUTINE    MEMOVE   (A   ,B   ,N   )
C
C     ARRAY MOVE FROM B TO A
C
      DIMENSION     A(1)     ,B(1)
C
      DO 100 I=1,N
      B(I)   =A(I)
  100 CONTINUE
C
      RETURN
      END
