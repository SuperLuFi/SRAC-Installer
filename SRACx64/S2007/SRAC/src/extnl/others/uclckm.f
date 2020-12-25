      SUBROUTINE UCLCKM(ICPU)
C
C     UCLCKM RETURN CURRENT USED CPU TIME (SEC) IN 4-BYTE INTEGER
C     This function is system dependent, so zero is set in ICPU here.
C     As a result, restart calculation option is not available.
C     If there is a similar function in your system, replace
C     the following statements.
C
      ICPU = 0 
      RETURN
      END
