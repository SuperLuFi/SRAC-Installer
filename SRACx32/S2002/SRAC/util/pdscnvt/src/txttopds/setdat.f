C-----------------------------------------------------------------------
C     DIMENSION WORK(6),IWORK(1)
C     EQUIVALENCE (WORK(1),IWORK(1))
C     CHARACTER LINE*72, MEMBER*8
C     LINE = ' 1.00000E+02-2.00000E-02        125 -3.00000E-12'
C     LD = 4
C     CALL SETDAT(LINE,LD,WORK)
C     WRITE(6,*) (WORK(I),I=1,LD)
C     WRITE(6,*) (IWORK(I),I=1,LD)
C     STOP
C     END
C-----------------------------------------------------------------------
C     SETDAT : SET DATA FROM TEXT LINE(A72) INCLUDING INTEGER OR
C              FLOATING NUMBER
C              SAMPLE:
C               1.00000E+02-2.00000E-02        125 -3.00000E-12 .......
C-----------------------------------------------------------------------
      SUBROUTINE SETDAT(LINE,LD,DATA)
C
      CHARACTER LINE*72,ADATA*12
      DIMENSION DATA(6),WORK(6),IWORK(1),ADATA(6)
      EQUIVALENCE (WORK(1),IWORK(1))
C
      DO 100 I=1,LD
        IS = 12*(I-1)+1
        IE = IS + 12 -1
        ADATA(I) = LINE(IS:IE)
        CALL TXTCHK(ADATA(I),ICODE)
        IF(ICODE.EQ.0) THEN
          READ(ADATA(I),'(1PE12.5)') WORK(I)
C         WRITE(6,*) I,WORK(I)
        ELSE
          CALL CNVINT(ADATA(I),IWORK(I))
        ENDIF
  100 CONTINUE
      DO 200 I=1,LD
        DATA(I) = WORK(I)
  200 CONTINUE
      RETURN
      END
