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
      ADATE = '61-02-09'
      RETURN
      END
