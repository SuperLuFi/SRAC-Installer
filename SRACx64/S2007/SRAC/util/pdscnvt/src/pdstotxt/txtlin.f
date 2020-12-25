C-----------------------------------------------------------------------
C     LENG = 18
C     CALL TXTLIN(LENG,LINE)
C     WRITE(6,*) LINE
C     STOP
C     END
C-----------------------------------------------------------------------
C     TXTLIN : SET LINE NUMBER OF PRINTED PDS DATA IN TEXT P-LIB
C              FORMAT FROM LENG
C             *PUT MU050000 N     14
C              1.00000E+02 2.00000E-02-3.00000E-04.........
C              1.00000E+02 2.00000E-02-3.00000E-04.........
C             -1.00000E+02 2.00000E-02
C              LENG=14 => LINES TO BE READ = 3
C-----------------------------------------------------------------------
      SUBROUTINE TXTLIN(LENG,LINE)
      IF(LENG.LE.0) THEN
        WRITE(6,*) ' ERROR(TXTLIN):LENGTH OF MEMBER = 0'
        STOP
      ENDIF
      LINE = LENG/6
      IF(MOD(LENG,6).NE.0) THEN
        IADD = 1
      ELSE
        IADD = 0
      ENDIF
      LINE = LINE + IADD
      RETURN
      END
