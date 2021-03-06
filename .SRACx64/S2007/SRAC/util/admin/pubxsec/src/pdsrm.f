C***********************************************************************
C UTILITY PROGRAM TO DELETE A MEMBER IN A PDS
C***********************************************************************
C
      SUBROUTINE PDSRM(DIRNAM,MEMBER,IRC)
C
C-----------------------------------------------------------------------
C     IRC : ERROR CODE
C         = 0 NORMAL END
C         = 1 MEMBER NOT FOUND 
C             (NOT EFFECTIVE : TREATED AS MEMBER IS DELETED)
C         = 2 MEMBER NAME IS EMPTY OR INVALID
C         = 3 DIRECTORY NAME IS EMPTY OR TOO LONG
C         = 4 MEMBER ALREADY EXIST IN WRITE MODE (NOT EFFECTIVE)
C         = 5 PDS OPEN ERROR
C         = 6 PDS CLOSE ERROR
C         = 7 PDS READ ERROR (NOT EFFECTIVE)
C         = 8 PDS WRITE ERROR (NOT EFFECTIVE)
C-----------------------------------------------------------------------
C
      COMMON /PDSDEV/ IOPDS
      CHARACTER DIRNAM*120,MEMBER*8,FILNAM*129 
C
      IF(IOPDS.LE.0) IOPDS = 41
      IRC = 0
C
C+++  SET FULL FILE NAME 
      CALL PDSNM(DIRNAM,MEMBER,FILNAM,NLENG,IERR)
      IF(IERR.EQ.1) THEN
        IRC = 2
        RETURN
      ELSEIF(IERR.GE.2) THEN
        IRC = 3
        RETURN
      ENDIF
C
C+++  CHECK FILE EXIST OR NOT
      CALL PDSSR(FILNAM,IEXST)
      IF(IEXST.EQ.1) THEN
        RETURN
      ENDIF
      LN = INDEX(FILNAM,' ') - 1 
      OPEN(UNIT=IOPDS, FILE=FILNAM(1:LN), ERR=100, IOSTAT=IOS)
      CLOSE(UNIT=IOPDS, ERR=200, STATUS='DELETE',IOSTAT=IOS)
      RETURN
  100 IRC = 5
      RETURN
  200 IRC = 6
      RETURN
      END
