C***********************************************************************
C UTILITY PROGRAM TO GIVE FULL NAME OF PDS MEMBER                      *
C FILNAM = DIRNAM/MEMBER                                               *
C***********************************************************************
C
      SUBROUTINE PDSNM(DIRNAM,MEMBER,FILNAM,NLENG,IERR)
C
      CHARACTER DIRNAM*120,MEMBER*8,FILNAM*129
      CHARACTER MEMWRK*9
C
C-------------------------------INPUT-----------------------------------
C     DIRNAM     : DIRECTORY NAME (A120) OF PDS : /XXX/XXX/MACRO
C     MEMBER     : PDS MEMBER NAME (A8) : CASEA010
C-------------------------------OUTPUT----------------------------------
C     FILNAM     : FULL FILE NAME (A129) OF PDS MEMBER
C                  /XXX/XXX/MACRO/CASEA010
C     NLENG      : LENGTH OF FILE NAME
C     IERR       : ERROR CODE =0 : NORMAL END
C                             =1 : MEMBER NAME IS EMPTY OR INVALID
C                             =2 : DIRECTORY NAME IS EMPTY OR INVALID
C                             =3 : FILE NAME IS TOO LONG
C-----------------------------------------------------------------------
C
      IERR = 0
      FILNAM = ' '
C
      NLENG = INDEX(DIRNAM,' ') - 1
      IF(NLENG.LE.0) THEN
        IERR = 2
        RETURN
      ENDIF
C
      MEMWRK = MEMBER//' '
      MLENG = INDEX(MEMWRK,' ') - 1
      IF(MLENG.LE.0) THEN
        IERR = 1
        RETURN
      ENDIF
C
      NLENGW = NLENG
      NLENG  = NLENG + 1 + MLENG
      IF(NLENG.GT.129) THEN
        IERR = 3
        RETURN
      ENDIF
      FILNAM(1:NLENG)=DIRNAM(1:NLENGW)//'/'//MEMBER
      RETURN
      END
