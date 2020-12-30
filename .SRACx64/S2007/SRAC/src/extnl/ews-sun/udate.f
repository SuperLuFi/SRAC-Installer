      subroutine udate(char_date)
C  For Sun-EWS
C --------------------------------------------------
C  output current date in 8-byte character       
C --------------------------------------------------
C  char_date : yy-mm-dd
C              yy : last two numeral in year (1995=>95) 
C              mm : two numeral for month    (Feb.=> 2)
C              dd : two numelal for day      ( 19 =>19)
C                   output char_date = 95- 2-19
C --------------------------------------------------
C  This subroutine corresponds to the FACOM survice 
C        routine "DATE".
C --------------------------------------------------
C  Here, SunFortran Survice routine(idate) is used.
C --------------------------------------------------
C
      character *8    char_date
      character *10   char_buff
      integer *4      iarray(3)
C
      call idate(iarray)
      write (char_buff, 100) (iarray(j), j=3,1,-1)
  100 format(i4, '-', i2, '-', i2)
      char_date = char_buff(3:10)
      return
      end
