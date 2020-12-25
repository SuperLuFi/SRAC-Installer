C***********************************************************************
C UTILITY PROGRAM TO SEARCH PDS FILE                                   *
C***********************************************************************
C
      SUBROUTINE PDSSC(DIRNAM,MEMBER,IEXST,IRC)
C
      CHARACTER DIRNAM*120,MEMBER*8,FILNAM*129
C
C-----------------------------------------------------------------------
C     IEXST      : = 0  EXIST
C                  = 1  NOT EXIST
C
C     IRC : ERROR CODE
C         = 0 NORMAL END
C         = 1 MEMBER NOT FOUND IN READ MODE (NOT EFFECTIVE)
C         = 2 MEMBER NAME IS EMPTY OR INVALID 
C         = 3 DIRECTORY NAME IS EMPTY OR TOO LONG
C         = 4 MEMBER ALREADY EXIST IN WRITE MODE (NOT EFFECTIVE)
C         = 5 PDS OPEN ERROR (NOT EFFECTIVE)
C         = 6 PDS CLOSE ERROR (NOT EFFECTIVE)
C         = 7 PDS READ ERROR (NOT EFFECTIVE)
C         = 8 PDS WRITE ERROR (NOT EFFECTIVE)
C-----------------------------------------------------------------------
C
      IRC = 0
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
      RETURN
      END
