      SUBROUTINE UCLCKM(ICPU)
C
C     UCLCKM RETURN CURRENT USED CPU TIME (SEC) IN 4-BYTE INTEGER
C     This function is system dependent, so zero is set in ICPU here.
C     As a result, restart calculation option is not available.
C     If there is a similar function in your system, replace
C     the following statements.
C
CMSASA. use C-coded one if you have POSIX/ANSI-C conformant C-compiler.
C
      real dt
      call cputime(dt) 
c
c  ... in mili-sec
      ICPU = dt *1000
      RETURN
      END
