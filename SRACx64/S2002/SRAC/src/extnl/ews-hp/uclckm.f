      subroutine uclckm(icpu)
C  For HP-EWS
C --------------------------------------------------
C  uclckm is originally a facom builtin service routine to return
C  current used cpu time(mili-sec) in 4-byte integer.
C
C  icpu(output)  : current used cpu time (mili-sec) in 4-byte integer
C
C --------------------------------------------------
C  Here, HP Fortran Survice routine(etime) is used.
C  icpu is a user time + system time.
C --------------------------------------------------
C
      real*4      e, etime, t(2)
      integer*4   icpu
C
      e = etime(t)
C     e:elapsed time(sec),
C     t(1): user time(sec)
C     t(2): system time(sec)
C
      icpu = int( (t(1)+t(2))*1000. )  
      return
      end
