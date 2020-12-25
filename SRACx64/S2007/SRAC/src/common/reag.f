      SUBROUTINE REAG(V,NCOUNT,NAME1,NAME2)
C =====================================================================
C    REAG
C =====================================================================
C
C
      REAL V(*)
      CHARACTER*4   NAME1, NAME2
C
      WRITE(6,85) NAME1,NAME2
   85 FORMAT(/11X,2A4)
C
      CALL REAM0( 'REAG', '    ', DUMMY2, V,  0, 0, NCOUNT )
      RETURN
      END
