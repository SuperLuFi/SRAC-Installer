C***********************************************************************
C UTILITY PROGRAM TO READ/WRITE CONTENTS OF PDS FILE                   *
C PDSIN / PDSOUT(ENTRY)                                                *
C***********************************************************************

      SUBROUTINE PDSIN(DIRNAM,MEMBER,WORK,LENG,IRC,IOUT)
C
C-----------------------------------------------------------------------
C     IRC : ERROR CODE
C         = 0 NORMAL END
C         = 1 MEMBER NOT FOUND
C         = 2 MEMBER NAME IS EMPTY
C         = 3 DIRECTORY NAME IS EMPTY
C         = 4 MEMBER ALREADY EXIST IN WRITE MODE
C         = 5 PDS OPEN ERROR
C         = 6 PDS CLOSE ERROR
C         = 7 PDS READ ERROR
C         = 8 PDS WRITE ERROR
C-----------------------------------------------------------------------
C
      DIMENSION WORK(1)
      CHARACTER DIRNAM*72,MEMBER*8,FILNAM*81
C
      IRC = 0
      CALL PDSNM(DIRNAM,MEMBER,FILNAM,NLENG,IERR)
      IF(IERR.NE.0) THEN
        IF(IERR.EQ.1) THEN
          IRC = 2
          WRITE(IOUT,*) ' XX MEMBER NAME IS EMPTY'
          RETURN
        ELSEIF(IERR.EQ.2) THEN
          IRC = 3
          WRITE(IOUT,*) ' XX DIRECTORY NAME IS EMPTY'
          RETURN
        ELSE
          RETURN
        ENDIF
      ENDIF
C
      CALL PDSSR(FILNAM,IEXST)
      IF(IEXST.EQ.1) THEN
        WRITE(IOUT,*) ' ** MEMBER ',MEMBER,' NOT FOUND IN ',DIRNAM
        RETURN
      ENDIF
C
      CALL PDSRD(FILNAM,WORK,LENG,IERR)
      IF(IERR.NE.0) THEN
        IF(IERR.EQ.1) THEN
          IRC = 5
          WRITE(IOUT,*) ' XX PDS OPEN ERROR'
          RETURN
        ELSEIF(IERR.EQ.2) THEN
          IRC = 6
          WRITE(IOUT,*) ' XX PDS CLOSE ERROR'
          RETURN
        ELSE
          IRC = 7
          WRITE(IOUT,*) ' XX PDS READ ERROR'
          RETURN
        ENDIF
      ELSE
        WRITE(IOUT,*) ' ** MEMBER  ',MEMBER,' WAS READ (',
     &                 LENG,' WORDS) FROM ',DIRNAM
        RETURN
      ENDIF
C
C=======================================================================
C
      ENTRY PDSOUT(DIRNAM,MEMBER,WORK,LENG,IRC,IOUT)
C
C=======================================================================
C
      IRC = 0
      CALL PDSNM(DIRNAM,MEMBER,FILNAM,NLENG,IERR)
      IF(IERR.NE.0) THEN
        IF(IERR.EQ.1) THEN
          IRC = 2
          WRITE(IOUT,*) ' XX MEMBER NAME IS EMPTY'
          RETURN
        ELSEIF(IERR.EQ.2) THEN
          IRC = 3
          WRITE(IOUT,*) ' XX DIRECTORY NAME IS EMPTY'
          RETURN
        ELSE
          RETURN
        ENDIF
      ENDIF
C
      CALL PDSSR(FILNAM,IEXST)
      IF(IEXST.EQ.0) THEN
        IRC = 4
        WRITE(IOUT,*) ' XX MEMBER ',MEMBER,' ALREADY EXIST IN ',DIRNAM
        RETURN
      ENDIF
C
      CALL PDSWT(FILNAM,WORK,LENG,IERR)
      IF(IERR.NE.0) THEN
        IF(IERR.EQ.1) THEN
          IRC = 5
          WRITE(IOUT,*) ' XX PDS OPEN ERROR'
          RETURN
        ELSEIF(IERR.EQ.2) THEN
          IRC = 6
          WRITE(IOUT,*) ' XX PDS CLOSE ERROR'
          RETURN
        ELSE
          IRC = 8
          WRITE(IOUT,*) ' XX PDS WRITE ERROR'
          RETURN
        ENDIF
      ELSE
        WRITE(IOUT,*) ' ** MEMBER  ',MEMBER,' WAS WRITTEN (',
     &                 LENG,' WORDS) IN ',DIRNAM
        RETURN
      ENDIF
      END
