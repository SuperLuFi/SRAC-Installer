C***********************************************************************
C UTILITY PROGRAM TO DELETE PDS MEMBER
C***********************************************************************

      SUBROUTINE PDSRM(FILNAM,IRC)
C
      CHARACTER FILNAM*81
C
C-------------------------------INPUT-----------------------------------
C     FILNAM     : FULL PDS MEMBER NAME : /XXX/XXX/MACRO/CASEA010
C-------------------------------OUTPUT----------------------------------
C     IRC        : ERROR CODE
C                = 0 NORMAL END
C                = 1 MEMBER NOT FOUND IN READ MODE (NOT EFFECTIVE)
C                = 2 MEMBER NAME IS EMPTY OR INVALID (NOT EFFECTIVE)
C                = 3 DIRECTORY NAME IS EMPTY OR INVALID (NOT EFFECTIVE)
C                = 4 MEMBER ALREADY EXIST IN WRITE MODE (NOT EFFECTIVE)
C                = 5 PDS OPEN ERROR
C                = 6 PDS CLOSE ERROR
C                = 7 PDS READ ERROR (NOT EFFECTIVE)
C                = 8 PDS WRITE ERROR (NOT EFFECTIVE)
C-----------------------------------------------------------------------
C
      IRC = 0
      IOPDS = 49
C
      OPEN(UNIT=IOPDS, FILE=FILNAM, ERR=100, IOSTAT=IOS)
      CLOSE(UNIT=IOPDS, ERR=200, STATUS='DELETE',IOSTAT=IOS)
      RETURN
  100 IRC = 5
      WRITE(6,*)    ' XX PDS OPEN ERROR(PDSRM) IN ',FILNAM
      RETURN
  200 IRC = 6
      WRITE(6,*)    ' XX PDS CLOSE ERROR(PDSRM) IN ',FILNAM
      RETURN
      END
