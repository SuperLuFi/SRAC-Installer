C***********************************************************************
C UTILITY PROGRAM TO GET LENGTH(WORDS) OF PDS DATA                     *
C***********************************************************************

      SUBROUTINE PDSLN(DIRNAM,MEMBER,LENG,IRC,IOUT)
C
C-----------------------------------------------------------------------
C     IRC : ERROR CODE
C         = 0 NORMAL END
C         = 1 MEMBER NOT FOUND
C         = 2 MEMBER NAME IS EMPTY OR INVALID
C         = 3 DIRECTORY NAME IS EMPTY OR INVALID
C         = 4 MEMBER ALREADY EXIST IN WRITE MODE (NOT EFFECTIVE)
C         = 5 PDS OPEN ERROR
C         = 6 PDS CLOSE ERROR
C         = 7 PDS READ ERROR
C         = 8 PDS WRITE ERROR (NOT EFFECTIVE)
C-----------------------------------------------------------------------
C
      CHARACTER DIRNAM*72,MEMBER*8,FILNAM*81
C
      IOPDS = 49
      IRC   = 0
      CALL PDSNM(DIRNAM,MEMBER,FILNAM,NLENG,IERR)
      IF(IERR.NE.0) THEN
        IF(IERR.EQ.1) THEN
          IRC = 2
          WRITE(IOUT,*) ' XX MEMBER NAME IS INVALID:',MEMBER
          if(iout.ne.6) then 
          WRITE(6,*)    ' XX MEMBER NAME IS INVALID:',MEMBER
          endif
          RETURN
        ELSEIF(IERR.EQ.2) THEN
          IRC = 3
          WRITE(IOUT,*) ' XX DIRECTORY NAME IS INVALID:',DIRNAM
          if(iout.ne.6) then 
          WRITE(6,*)    ' XX DIRECTORY NAME IS INVALID:',DIRNAM
          endif
          RETURN
        ELSE
          RETURN
        ENDIF
      ENDIF
C
      CALL PDSSR(FILNAM,IEXST)
      IF(IEXST.EQ.1) THEN
        WRITE(IOUT,*) ' ** MEMBER ',MEMBER,' NOT FOUND IN ',DIRNAM
        IRC = 1
        RETURN
      ENDIF
C
      OPEN(UNIT=IOPDS, FILE=FILNAM, ERR=100, STATUS='UNKNOWN',
     &     ACCESS='SEQUENTIAL', FORM='UNFORMATTED',IOSTAT=IOS)
      READ(UNIT=IOPDS,ERR=300,IOSTAT=IOS)  LENG
      CLOSE(UNIT=IOPDS, ERR=200, STATUS='KEEP',IOSTAT=IOS)
      GOTO 9999
C
  100 IRC = 5
      WRITE(IOUT,*) ' XX PDS OPEN ERROR IN ',FILNAM
      if(iout.ne.6) then 
      WRITE(6,*)    ' XX PDS OPEN ERROR IN ',FILNAM
      endif
      RETURN
  200 IRC = 6
      if(iout.ne.6) then 
      WRITE(IOUT,*) ' XX PDS CLOSE ERROR IN ',FILNAM
      endif
      WRITE(6,*)    ' XX PDS CLOSE ERROR IN ',FILNAM
      RETURN
  300 IRC = 7
      WRITE(IOUT,*) ' XX PDS READ ERROR IN ',FILNAM
      if(iout.ne.6) then 
      WRITE(6,*)    ' XX PDS READ ERROR IN ',FILNAM
      endif
      RETURN
 9999 RETURN
      END
