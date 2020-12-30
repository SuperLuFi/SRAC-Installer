C     CHARACTER*12 ADATA
C     ADATA = '     125    '
C     ADATA = '125         '
C     ADATA = '         125'
C     ADATA = '            '
C     CALL CNVINT(ADATA,IDATA)
C     WRITE(6,*) IDATA
C     STOP
C     END
C-----------------------------------------------------------------------
C     CNVINT : SUBROUTINE TO CONVERT TEXT DATA(A12) INTO INTEGER DATA
C              EX : '   1234     ' => 1234
C-----------------------------------------------------------------------
      SUBROUTINE CNVINT(ADATA,IDATA)
      CHARACTER*12 ADATA,ATEMP
C
      ATEMP = '            '
      IS = 0
      IE = 0
      DO 100 I=1,12
        IF(ADATA(I:I).NE.' '.AND.IS.EQ.0) THEN
          IS = I
          IE = 0
          IF(I.EQ.12) IE = IS
          GOTO 100
        ENDIF
        IF(ADATA(I:I).EQ.' '.AND.IE.EQ.0.AND.IS.NE.0) THEN
          IE = I-1
          GOTO 100
        ENDIF
        IF(ADATA(I:I).NE.' '.AND.I.EQ.12) THEN
          IE = 12
          GOTO 100
        ENDIF
        IF(I.EQ.12.AND.IS.EQ.0) THEN
          WRITE(6,*) ' ERROR(CNVINT) : DATA MAY BE EMPTY :',ADATA
          STOP
        ENDIF
  100 CONTINUE
      LDATA  = IE-IS+1
      LS = 12-LDATA+1
      LE = 12
      ATEMP(LS:LE) = ADATA(IS:IE)
      READ(ATEMP,'(I12)') IDATA
      RETURN
      END
