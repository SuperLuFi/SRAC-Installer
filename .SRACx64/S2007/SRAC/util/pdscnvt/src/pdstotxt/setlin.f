C-----------------------------------------------------------------------
C     DIMENSION WORK(6),IWORK(1)
C     EQUIVALENCE (WORK(1),IWORK(1))
C     CHARACTER LINE*72
C     WORK(1)  =  1.00000E+02
C     WORK(2)  = -2.00000E-02
C     IWORK(3) = 125 -3.00000E-12
C     WORK(4)  = -3.00000E-12
C     IWORK(5) = 1
C     WORK(6) = -4.125E-1
C     LD = 3
C     CALL SETLIN(LINE,LD,WORK)
C     WRITE(6,*) LINE
C     STOP
C     END
C-----------------------------------------------------------------------
C     SETLIN : SET DATA INCLUDING INTEGER OR FLOATING NUMBER
C              IN TEXT LINE(A72)
C              SAMPLE:
C               1.00000E+02-2.00000E-02        125 -3.00000E-12 .......
C-----------------------------------------------------------------------
      SUBROUTINE SETLIN(LINE,LD,DATA)
C
      CHARACTER LINE*72
      DIMENSION DATA(6),WORK(6),IWORK(1)
      EQUIVALENCE (WORK(1),IWORK(1))
C
      LINE = ' '
      DO 100 I=1,LD
        WORK(I) = DATA(I)
  100 CONTINUE
C
      DO 200 I=1,LD
        IS = 12*(I-1)+1
        IE = IS + 12 -1
        CALL NUMCHK(DATA(I),ICODE)
        IF(ICODE.EQ.0) THEN
          WRITE(LINE(IS:IE),'(1PE12.5)') WORK(I)
        ELSE
          WRITE(LINE(IS:IE),'(4X,I7,1X)') IWORK(I)
        ENDIF
  200 CONTINUE
      RETURN
      END
