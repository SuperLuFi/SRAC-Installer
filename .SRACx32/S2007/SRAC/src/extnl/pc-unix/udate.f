      SUBROUTINE UDATE(ADATE)
C
C     DATE IS FACOM BUILTIN SERVICE ROUTINE TO RETURN CURRENT DATE.
C     ADATE(OUTPUT) : CURRENT DATE IN 8-BYTE
C                     EX. '95-02-09' IF WRITE ADATE IN A8 FORMAT
C     This routine is not effective on calculated result. It is used as
C     just comment.
C     Now, my birthday is set in ADATE. If there is a similar function 
C     in your system, replace the following statements. 
C
      CHARACTER*8 ADATE
C     CALL DATE(ADATE)
c     ADATE = '61-02-09'
C
CMSASA. use C-coded one if you have POSIX/ANSI-C conformant C-compiler.
C
      call cctimdat(ih,imin,isec, id,im, iy ) 
CKSK---- for 2000 year problem ---
      itmp = iy / 100
      iy = iy - (itmp*100)
      write(adate,'(i2,''-'',i2,''-'',i2)') iy, im, id
c
      RETURN
      END
