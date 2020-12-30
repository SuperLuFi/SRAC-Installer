      SUBROUTINE  DCLEA( A , IMAX , DSET)
C
      REAL*8     A(IMAX),DSET
C
      DO 100 I = 1 , IMAX
      A(I)     = DSET
  100 CONTINUE
C
      RETURN
      END
