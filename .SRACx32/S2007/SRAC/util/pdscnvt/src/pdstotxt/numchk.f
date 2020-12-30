C-----------------------------------------------------------------------
C     NUMCHK : SUBROUTINE TO CHECK A BINARY DATA IS INTEGER OR NOT
C     ICODE = 0 : FLOATING NUMBER
C           = 1 : INTEGER NUMBER
C-----SAMPLE PROGRAM----------------------------------------------------
C
C     DIMENSION WORK(10),IWORK(10)
C     EQUIVALENCE (WORK(1),IWORK(1))
C     DO 100 I=1,10
C       WORK(I) = FLOAT(I)
C       IF (I.EQ.10) IWORK(I) = 10
C       CALL NUMCHK (WORK(I),ICODE)
C       IF(ICODE.EQ.1) THEN
C         WRITE(6,*) IWORK(I)
C       ELSE
C         WRITE(6,*) WORK(I)
C       ENDIF
C 100 CONTINUE
C     STOP
C     END
C-----------------------------------------------------------------------
      SUBROUTINE NUMCHK (IA,ICODE)
      CHARACTER AHO*7
C
      WRITE(AHO,'(I7)') IA
      IF(AHO.EQ.'*******') THEN
        ICODE = 0
      ELSE
        ICODE = 1
      ENDIF
C     WRITE(6,*) AHO
      RETURN
      END
