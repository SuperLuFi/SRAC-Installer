C
C
C =====================================================================
C    REAI
C =====================================================================
C
      SUBROUTINE REAI(IV,NCOUNT,NAME1,NAME2)
C
      INTEGER IV(*)
      CHARACTER*4   NAME1, NAME2
C
      WRITE(6,85) NAME1,NAME2
   85 FORMAT(/11X,2A4)
C
      CALL REAM0( 'REAI', '    ', IV, DUMMY2,  0, NCOUNT, 0 )
      RETURN
      END
