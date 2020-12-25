      subroutine udate(chdate)
C  For HP-EWS
C --------------------------------------------------
C  output current date in 8-byte character
C --------------------------------------------------
C  char_date : yy-mm-dd
C              yy : last two numeral in year (1995=>95)
C              mm : two numeral for month    (Feb.=> 2)
C              dd : two numelal for day      (  9 =>19)
C                   output chdate = 95- 2-19
C --------------------------------------------------
C  This subroutine corresponds to the FACOM survice
C        routine "DATE".
C --------------------------------------------------
C  Here, HP Fortran Survice routine(idate) is used.
C
C       idate(month,iday,iyear)      :  VMS Version 
C             month 1 to 12
C             iday  1 to 31
C             iyear 00 to 99
C  Cautin please compiled with ether the +e or +E1 option.
C --------------------------------------------------
C
      character *8    chdate
      integer *4      month,iday,iyear
C
      call idate(month,iday,iyear)
      iyear = mod(iyear,100)
      write (chdate, 100) iyear,month,iday
  100 format(i2, '-', i2, '-', i2)
      return
      end
