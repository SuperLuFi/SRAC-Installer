C
C     TOTAL FACTOR SETTING SUBROUTINE.
C          '81-1-26 BY ONUMA
C
      SUBROUTINE HFACTR(FACT)
      COMMON /GCOMXY/ I(4)
      COMMON /GCOMXY/ FCT00, G(6)
C      
      DATA IST/0/
C      
      IF( I(1) .NE. 9999 .AND. IST .EQ. 0 ) THEN
         FCT00 = 1.0
      ENDIF
C      
      IF( IST .NE. 1 ) THEN
         IST = 1
         FCT00 = FCT00*FACT
         CALL FACTOR( 1.0 )
      ENDIF
C      
      CONTINUE
      RETURN
C
      ENTRY SEEHFC( F1 )
      F1 = FCT00
      RETURN
C
      ENTRY SEEORG( X, Y )
      X = G(4)
      Y = G(5)
      RETURN
C
      ENTRY SEEPEN( IP )
      IP = I(2)
      RETURN
      END
