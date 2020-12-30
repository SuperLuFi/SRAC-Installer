C***********************************************************************
C UTILITY PROGRAM TO GET LENGTH(WORDS) OF PDS DATA                     *
C***********************************************************************
C
      SUBROUTINE PDSLN(DIRNAM,MEMBER,LENG,IRC)
C
C-----------------------------------------------------------------------
C     IRC : ERROR CODE
C         = 0 NORMAL END
C         = 1 MEMBER NOT FOUND
C         = 2 MEMBER NAME IS EMPTY OR INVALID
C         = 3 DIRECTORY NAME IS EMPTY OR TOO LONG
C         = 4 MEMBER ALREADY EXIST IN WRITE MODE (NOT EFFECTIVE)
C         = 5 PDS OPEN ERROR
C         = 6 PDS CLOSE ERROR
C         = 7 PDS READ ERROR
C         = 8 PDS WRITE ERROR (NOT EFFECTIVE)
C-----------------------------------------------------------------------
C
      COMMON /PDSDEV/ IOPDS
      CHARACTER DIRNAM*120,MEMBER*8,FILNAM*129
C
      IF(IOPDS.LE.0) IOPDS = 41
      IRC   = 0
      CALL PDSNM(DIRNAM,MEMBER,FILNAM,NLENG,IERR)
      IF(IERR.EQ.1) THEN
        IRC = 2
        RETURN
      ELSEIF(IERR.GE.2) THEN
        IRC = 3
        RETURN
      ENDIF
C
      CALL PDSSR(FILNAM,IEXST)
      IF(IEXST.EQ.1) THEN
        IRC = 1
        RETURN
      ENDIF
C
      LN = INDEX(FILNAM,' ') - 1
      OPEN(UNIT=IOPDS, FILE=FILNAM(1:LN), ERR=100, STATUS='UNKNOWN',
     &     ACCESS='SEQUENTIAL', FORM='UNFORMATTED',IOSTAT=IOS)
      READ(UNIT=IOPDS,ERR=300,IOSTAT=IOS)  LENG
      CLOSE(UNIT=IOPDS, ERR=200, STATUS='KEEP',IOSTAT=IOS)
      GOTO 9999
C
  100 IRC = 5
      RETURN
  200 IRC = 6
      RETURN
  300 IRC = 7
      RETURN
 9999 RETURN
      END
