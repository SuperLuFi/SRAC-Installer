CCC
      SUBROUTINE BSHIFT(A,II,N)
C ------- SHIFT N CHARCTER IN II.  ( +N : RIGHT, -N : LEFT )
C         FILL UP THE MARGIN BY BLANKS.
      CHARACTER*4 A,II
      IF(N.EQ.0) THEN
          A = II
          RETURN
      ENDIF
      A = '    '
      IF(ABS(N).GE.4) RETURN
      IF(N.GT.0) THEN
          A(N+1:4) = II(1:4-N)
      ELSE
          A(1:4+N) = II(-N+1:4)
      ENDIF
      RETURN
      END
