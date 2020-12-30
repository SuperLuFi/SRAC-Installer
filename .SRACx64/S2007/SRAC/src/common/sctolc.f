C**************************************************************
C*   CHANGE SMALL CHARACTER TO LARGE CHARACTER
C*   CALLED FROM UGSCHA ROUTINE
C**************************************************************
      SUBROUTINE SCTOLC(ICHAR,M)
      DIMENSION ICHAR(18)
      CHARACTER  SMALL*26, LARGE*26, ICHAR*4
C
      SMALL = 'abcdefghijklmnopqrstuvwxyz'
      LARGE = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
C
      DO 100 I=1,M
        DO 100 J=1,4
          DO 100 K=1,26
            IF(ICHAR(I)(J:J).EQ.SMALL(K:K)) ICHAR(I)(J:J)=LARGE(K:K)
  100 CONTINUE
      RETURN
      END
