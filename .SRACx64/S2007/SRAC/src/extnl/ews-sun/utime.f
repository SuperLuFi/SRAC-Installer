      subroutine utime(jtime)
C  For Sun-EWS
C --------------------------------------------------
C  Output current time(mili-sec) in the current date in mili-second
C         from am 0:00.
C --------------------------------------------------
C  itime  : output (mili-sec)
C           ex.  PM 2:09:45  => jtime = (3600*14+60*9+45)*1000 
C --------------------------------------------------
C  This subroutine corresponds to the FACOM survice 
C        routine "TIME".
C --------------------------------------------------
C  Here, SunFortran Survice routine(itime) is used.
C --------------------------------------------------
C
      integer*4 iarray(3)
C
      call itime(iarray)
      jtime = (iarray(1)*3600+iarray(2)*60+iarray(3))*1000
      return
      end
