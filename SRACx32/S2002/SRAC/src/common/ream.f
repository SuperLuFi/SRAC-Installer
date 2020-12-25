C
C
C =====================================================================
C    REAM
C =====================================================================
C
      SUBROUTINE REAM( AV, IV, V,  NCA, NCI, NCR )
C
      IMPLICIT REAL*8 (D)
C
      CHARACTER*4 AV(*)
      INTEGER     IV(*)
      REAL        V(*)
C
C
      CALL REAM0( 'REAM', AV, IV, V, NCA, NCI, NCR )
C
      RETURN
      END
