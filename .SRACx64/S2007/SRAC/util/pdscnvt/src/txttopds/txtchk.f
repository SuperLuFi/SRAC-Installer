C     CHARACTER*12 ADATA
C     ADATA = ' 1.00000E-04'
C     ADATA = ' 0.         '
C     ADATA = '         12 '
C     CALL TXTCHK(ADATA,ICODE)
C     WRITE(6,*) ICODE
C     STOP
C     END
C-----------------------------------------------------------------------
C     TXTCHK : SUBROUTINE TO CHECK A TEXT DATA(A12) IS INTEGER OR NOT
C     ICODE = 0 : FLOATING NUMBER
C           = 1 : INTEGER NUMBER
C-----------------------------------------------------------------------
      SUBROUTINE TXTCHK(ADATA,ICODE)
      CHARACTER*12 ADATA
      DO 100 I=1,12
        IF(ADATA(I:I).EQ.'.') THEN
          ICODE = 0
          RETURN
        ELSE
          ICODE = 1
        ENDIF
  100 CONTINUE
      RETURN
      END
