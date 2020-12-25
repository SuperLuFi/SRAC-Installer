C***********************************************************************
C UTILITY PROGRAM TO READ CONTENTS OF PDS FILE                         *
C***********************************************************************
C
      SUBROUTINE PDSIN(DIRNAM,MEMBER,WORK,LENG,IOUT,IPRN,IRC)
C
C-----------------------------------------------------------------------
C     IRC : ERROR CODE
C         = 0 NORMAL END
C         = 1 MEMBER NOT FOUND IN READ MODE
C         = 2 MEMBER NAME IS EMPTY OR INVALID
C         = 3 DIRECTORY NAME IS EMPTY OR TOO LONG
C         = 4 MEMBER ALREADY EXIST IN WRITE MODE
C         = 5 PDS OPEN ERROR
C         = 6 PDS CLOSE ERROR
C         = 7 PDS READ ERROR
C         = 8 PDS WRITE ERROR
C
C      LENG WILL BE REPLACED BY TOTAL DATA LENGTH IN MEMBER
C-----------------------------------------------------------------------
C
      DIMENSION WORK(*)
      CHARACTER DIRNAM*120,MEMBER*8,FILNAM*129
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
      IF(IEXST.EQ.1) THEN
        IRC = 1
        RETURN
      ENDIF
C
      LENGKP = LENG
      CALL PDSRD(FILNAM,WORK,LENG,IERR)
      IF(IERR.NE.0) THEN
        IF(IERR.EQ.1) THEN
          IRC = 5
          RETURN
        ELSEIF(IERR.EQ.2) THEN
          IRC = 6
          RETURN
        ELSE
          IRC = 7
          RETURN
        ENDIF
      ELSE
        IF(IPRN.GE.1) THEN
          IF(LENGKP.LT.0) THEN
            WRITE(IOUT,6000) '     MEMBER ',MEMBER,' WITH LENGTH ',
     &           LENG,' WAS PARTIALLY(',-LENGKP,') READ FROM ',DIRNAM
          ELSE
            WRITE(IOUT,6100) '     MEMBER ',MEMBER,' WITH LENGTH ',
     &           LENG,' WAS READ FROM ',DIRNAM
          ENDIF
        ENDIF
      ENDIF
 6000 FORMAT(A,A,A,I7,A,I7,A,/,5X,A)
 6100 FORMAT(A,A,A,I7,A,/,5X,A)
C
      RETURN
      END
