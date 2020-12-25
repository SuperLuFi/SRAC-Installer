      SUBROUTINE ICLEA( IA , LENGI , IB )
C***********************************************************************
C     ICLEA IS USED FOR VALUE SET OF INTEGER 1-DIMENSION ARRAY.
C***********************************************************************
      DIMENSION    IA(1)
C
      DO 200 I = 1 , LENGI
      IA(I) = IB
  200 CONTINUE
      RETURN
      END
