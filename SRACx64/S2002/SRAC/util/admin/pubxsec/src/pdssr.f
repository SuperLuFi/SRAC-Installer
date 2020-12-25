C***********************************************************************
C UTILITY PROGRAM TO SEARCH PDS FILE                                   *
C FILNAM = DIRNAM/MEMBER                                               *
C***********************************************************************
C
      SUBROUTINE PDSSR(FILNAM,IEXST)
C
      CHARACTER FILNAM*129
C
C-------------------------------INPUT-----------------------------------
C     FILNAM     : FULL FILE NAME (A129) OF PDS FILE
C                  /XXX/XXX/MACRO/CASEA010
C-------------------------------OUTPUT----------------------------------
C     IEXST      : = 0  EXIST
C                  = 1  NOT EXIST
C-----------------------------------------------------------------------
C
      LOGICAL EX
      LN = INDEX(FILNAM,' ') - 1
      INQUIRE(FILE=FILNAM(1:LN),EXIST=EX)
      IF(.NOT.EX) THEN
        IEXST = 1
      ELSE
        IEXST = 0
      ENDIF
      RETURN
      END
