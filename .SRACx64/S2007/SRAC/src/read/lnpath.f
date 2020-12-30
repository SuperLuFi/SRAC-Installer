      SUBROUTINE  LNPATH(LINE,PATH,FLMODE,DATAKP,LENG)
C
      CHARACTER*72   LINE
      CHARACTER*68   PATH
      CHARACTER*8    FLMODE,DATAKP
C
C 10  READ(5,'(A72)',END=999) LINE
C
      LENG = 0
CKSK  PATH = 68H
CKSK +
      PATH = '                                  '//
     +       '                                  '
CKSK  FLMODE = 8H
      FLMODE = '        '
CKSK  DATAKP = 8H
      DATAKP = '        '
      IF(LINE(1:1).EQ.' ') GO TO 100
C
      IST    = 1
C
CM    IST    = 0
CM    DO 20 I = 1 , 72
CM    IF(LINE(I:I).NE.' ') THEN
CM                         IST = I
CM                         GO TO 30
CM                         ENDIF
CM 20 CONTINUE
CM    GO TO 100
C
   30 IEND =  72
      DO 40 I = IST , 72
      IF(LINE(I:I).EQ.' ') THEN
                           IEND = I-1
                           GO TO 50
                           ENDIF
   40 CONTINUE
   50 LENG = IEND - IST + 1
      IF(LENG.GT.68) LENG = 68
      IEND0 = IST + LENG -1
      PATH(1:LENG) = LINE(IST:IEND0)
C
      IST = IEND + 2
      IF(IST.GT.72) GO TO 100
C
      DO 60 I = IST ,  72
      IF(LINE(I:I).NE.' ') THEN
                           IPOS = I + 1
CKSK                       IF(LINE(I:I).EQ.'N') FLMODE = 8HNEW
CKSK                       IF(LINE(I:I).EQ.'O') FLMODE = 8HOLD
CKSK                       IF(LINE(I:I).EQ.'S') FLMODE = 8HSCRATCH
                           IF(LINE(I:I).EQ.'N') FLMODE = 'NEW     '
                           IF(LINE(I:I).EQ.'O') FLMODE = 'OLD     '
                           IF(LINE(I:I).EQ.'S') FLMODE = 'SCRATCH '
                           GO TO 70
                           ENDIF
   60 CONTINUE
      GO TO 100
C
   70 IST = IPOS
      IF(IST.GT.72) GO TO 100
      DO 80 I = IST , 71
      IPOS = I + 1
      IF(LINE(I:I).EQ.' '.AND.LINE(IPOS:IPOS).NE.' ') GO TO 90
   80 CONTINUE
      GO TO 100
C
   90 CONTINUE
      IF(IPOS.GT.72) GO TO 100
C
CKSK  IF(LINE(IPOS:IPOS).EQ.'F') DATAKP = 8HFILE
      IF(LINE(IPOS:IPOS).EQ.'F') DATAKP = 'FILE    '
CKSK  IF(LINE(IPOS:IPOS).EQ.'C') DATAKP = 8HCORE
      IF(LINE(IPOS:IPOS).EQ.'C') DATAKP = 'CORE    '
C
  100 CONTINUE
CM    WRITE(6,*) ' ** LENG PATH : ',LENG,PATH(1:LENG)
CM    WRITE(6,*) ' ** FLMODE DATAKP : ',FLMODE,DATAKP
CDEL  GO TO 10
C
C 999 STOP
C
      RETURN
      END
