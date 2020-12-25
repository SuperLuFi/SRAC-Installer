C***********************************************************************
C UTILITY PROGRAM TO GIVE FULL NAME OF PDS MEMBER                      *
C FILNAM = DIRNAM/MEMBER                                               *
C***********************************************************************

      SUBROUTINE PDSNM(DIRNAM,MEMBER,FILNAM,NLENG,IERR)
C
      CHARACTER DIRNAM*72,MEMBER*8,FILNAM*81
C
C-------------------------------INPUT-----------------------------------
C     DIRNAM     : DIRECTORY NAME (A72) OF PDS : /XXX/XXX/MACRO
C     MEMBER     : PDS MEMBER NAME (A8) : CASEA010
C-------------------------------OUTPUT----------------------------------
C     FILNAM     : FULL FILE NAME (A80) OF PDS MEMBER
C                  /XXX/XXX/MACRO/CASEA010
C     NLENG      : LENGTH OF FILE NAME
C     IERR       : ERROR CODE =0 : NORMAL END
C                             =1 : MEMBER NAME IS EMPTY OR INVALID
C                             =2 : DIRECTORY NAME IS EMPTY OR INVALID
C-----------------------------------------------------------------------
C
      IERR = 0
      IF(MEMBER(1:1).EQ.' ') THEN
        IERR = 1
        RETURN
      ENDIF
      IF(DIRNAM(1:1).EQ.' ') THEN
        IERR = 2
        RETURN
      ENDIF
C
      FILNAM = ' '
      NLENG = 0
      DO 100 I=1,72
        IF(DIRNAM(I:I).NE.' ') THEN
          NLENG = NLENG + 1
        ELSE
          GOTO 200
        ENDIF
  100 CONTINUE
  200 IF(NLENG.EQ.0) THEN
        IERR = 2
        RETURN
      ENDIF
C
      MLENG = 0
      DO 300 I=1,8
CKSK    IF(MEMBER(I:I).NE.' ') THEN
        IF(MEMBER(I:I).NE.' '.AND.MEMBER(I:I).NE.'\t') THEN 
          MLENG = MLENG + 1
        ELSE
          GOTO 400
        ENDIF
  300 CONTINUE
C
  400 IF(MLENG.EQ.0) THEN
        IERR = 1
        RETURN
      ENDIF
      NLENGW = NLENG
      NLENG  = NLENG + 1 + MLENG
      FILNAM(1:NLENG)=DIRNAM(1:NLENGW)//'/'//MEMBER
      RETURN
      END
