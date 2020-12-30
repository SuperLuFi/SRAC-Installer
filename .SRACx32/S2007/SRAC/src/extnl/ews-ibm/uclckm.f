      subroutine uclckm(icpu)
C  For IBM-EWS
C --------------------------------------------------
C  uclckm is originally a facom builtin service routine to return
C  current used cpu time(mili-sec) in 4-byte integer.
C
C  icpu(output)  : current used cpu time (mili-sec) in 4-byte integer
C
C --------------------------------------------------
C  Here, XL FORTRAN Survice routine(MCLOCK) is used.
C  MCLOCK return current used user & system time in 1/100 sec
C --------------------------------------------------
C
      integer*4   icpu
C
C
      icpu = MCLOCK()*10
      return
      end
