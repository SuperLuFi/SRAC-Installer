C
C     INTERFACE ROUTINE FOR PLOTGL
C
      SUBROUTINE GRPLOT( X, Y, IPEN )
C      
      COMMON /LTPCOM/ LTP001, LTP002
C      
      DATA ILOLD / 999 /
C      
      LNUM = 1
C      
      IF( LTP001 .EQ. 9999 ) THEN
         LNUM = LTP002
      ENDIF
C      
      LPEN = IABS(IPEN)
      IF( LPEN .GT. 100 ) THEN
         CALL PLOT( X, Y, IPEN )
         RETURN
      ENDIF
C      
      IF( ILOLD .EQ. 999 ) THEN
         CALL PLOTGL( X, Y, 0 )
         GO TO 3000
      ENDIF
C      
      IF( LPEN .EQ. 2 ) THEN
         IF( LNUM .NE. ILOLD ) THEN
            CALL PLOTGL( XX, YY, 1000 )
            CALL PLOTGL( XX, YY, 0 )
         ENDIF
         CALL PLOTGL( X, Y, LNUM )
      ELSEIF( LPEN .EQ. 3 ) THEN
         CALL PLOTGL( X, Y, 0 )
      ENDIF
C      
 3000 CONTINUE
C      
      IF( IPEN .LT. 0 ) THEN
         CALL PLOT( X, Y, -3 )
      ENDIF
C     
      ILOLD = LNUM
C      
      RETURN
      END
