C-----------------------------------------------------------------------
C     CHARACTER LINE*72, MEMBER*8
C     LINE = '*PUT CU050000 N   1234 '
C     CALL SETMEM(LINE,MEMBER,LENG)
C     WRITE(6,*) ' MEMBER  = ',MEMBER
C     WRITE(6,*) ' LENG    = ',LENG
C     STOP
C     END
C-----------------------------------------------------------------------
C     SETMEM : SET MEMBER NAME AND LENGTH FROM THE LINE DATA :
C              *PUT   CU050000  N    18
C-----------------------------------------------------------------------
      SUBROUTINE SETMEM(LINE,MEMBER,LENG)
      CHARACTER LINE*72, MEMBER*8, ALENG*10
C
      ALENG = '          '
      IF(LINE(1:4).NE.'*PUT') THEN
        WRITE(6,*) ' ERROR(SETMEM) : NOT COMMAND(*PUT) LINE '
        STOP
      ENDIF
C
      IC1S = 1
      IC1E = 0
      IC2S = 0
      IC2E = 0
      IC3S = 0
      IC3E = 0
      IC4S = 0
      IC4E = 0
      IC = 0
      DO 100 I=1,72
        IF(LINE(I:I).EQ.' '.AND.IC1E.EQ.0) THEN
          IC1E = I-1
          IF(IC1S.GT.IC1E) IC1E = IC1S
        ENDIF
        IF (IC1E.EQ.0) GOTO 100
        IF(LINE(I:I).NE.' '.AND.IC2S.EQ.0) THEN
          IC2S = I
        ENDIF
        IF(IC2S.EQ.0) GOTO 100
        IF(LINE(I:I).EQ.' '.AND.IC2E.EQ.0) THEN
          IC2E = I-1
          IF(IC2S.GT.IC2E) IC2E = IC2S
        ENDIF
        IF(IC2E.EQ.0) GOTO 100
        IF(LINE(I:I).NE.' '.AND.IC3S.EQ.0) THEN
          IC3S = I
        ENDIF
        IF(IC3S.EQ.0) GOTO 100
        IF(LINE(I:I).EQ.' '.AND.IC3E.EQ.0) THEN
          IC3E = I-1
          IF(IC3S.GT.IC3E) IC3E = IC3S
        ENDIF
        IF(IC3E.EQ.0) GOTO 100
        IF(LINE(I:I).NE.' '.AND.IC4S.EQ.0) THEN
          IC4S = I
        ENDIF
        IF(IC4S.EQ.0) GOTO 100
        IF(LINE(I:I).EQ.' '.AND.IC4E.EQ.0) THEN
          IC4E = I-1
          IF(IC4S.GT.IC4E) IC4E = IC4S
        ENDIF
  100 CONTINUE
C     WRITE(6,*) IC1S,IC1E
C     WRITE(6,*) IC2S,IC2E
C     WRITE(6,*) IC3S,IC3E
C     WRITE(6,*) IC4S,IC4E
      MEMBER = LINE(IC2S:IC2E)
      LLENG  = IC4E-IC4S+1
      LS = 10-LLENG+1
      LE = 10
      ALENG(LS:LE) = LINE(IC4S:IC4E)
      READ(ALENG,'(I10)') LENG
      RETURN
      END
