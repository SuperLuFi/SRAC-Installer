C             SAVEAF              LEVEL=1        DATE=81.11.14
      SUBROUTINE SAVEAF (AF,BF,Q,IQ,I,J,ITP,JT,MM)
C
C     SAVE ANGULAR FLUX ON BOUNDARY
C
      DIMENSION AF(ITP,MM), BF(JT,MM), Q(JT,MM)
C
C
      DO 100 M=1,MM
      T=BF(J,M)
      IF (IQ.NE.0) T=T-Q(J,M)
  100 AF(I,M)=T
      RETURN
      END
