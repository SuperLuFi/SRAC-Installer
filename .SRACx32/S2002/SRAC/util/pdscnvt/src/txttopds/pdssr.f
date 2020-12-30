C***********************************************************************
C UTILITY PROGRAM TO SEARCH PDS MEMBER                                 *
C FILNAM = DIRNAM/MEMBER                                               *
C***********************************************************************

      SUBROUTINE PDSSR(FILNAM,IEXST)
C
      CHARACTER FILNAM*81
C
C-------------------------------INPUT-----------------------------------
C     FILNAM     : FULL FILE NAME (A80) OF PDS MEMBER
C                  /XXX/XXX/MACRO/CASEA010
C-------------------------------OUTPUT----------------------------------
C     IEXST      : = 0  EXIST
C                  = 1  NOT EXIST
C-----------------------------------------------------------------------
C
      LOGICAL EX
      INQUIRE(FILE=FILNAM,EXIST=EX)
      IF(.NOT.EX) THEN
        IEXST = 1
      ELSE
        IEXST = 0
      ENDIF
      RETURN
      END
