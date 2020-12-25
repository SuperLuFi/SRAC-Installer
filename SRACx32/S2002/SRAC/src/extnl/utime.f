      SUBROUTINE UTIME(ITIME)
C
C     TIME IS FACOM BUILTIN SERVICE ROUTINE TO RETURN CURRENT TIME.
C     ITIME(OUTPUT) : CURRENT TIME IN THE CURRENT DATE IN MILI-SECOND
C                     FROM AM 0:00.
C     This routine is not effective on calculated result. It is used as
C     just comment.
C     Now, zero is set in ITIME. If there is a similar function in your
C     system, replace the following statements.
C
C     CALL TIME(ITIME)
c     ITIME = 0
C
CMSASA .. C-Coded routime (POSIX/ANSIC comformat C-compiler is necessary)
      call cctimdat( ih, imn, is, id, im, iy ) 
      itime = 1000*( 60*( 60*ih + imn) + is )
c
      RETURN
      END
