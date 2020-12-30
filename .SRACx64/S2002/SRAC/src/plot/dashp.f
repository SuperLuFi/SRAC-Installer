C*************************************************************************
C*    SUBROUTINE     DASHP                                               *
C*         04 CHAR   PLOT      EBCDIC    MM + CM + INCH                  *
C*************************************************************************
C*    CALL DASHP  (XPAGE,YPAGE,DASH)                                     *
C*                                                                       *
C*    XPAGE, YPAGE  ARE THE COORDINATES OF THE POINT TO WHICH THE DASHED *
C*                  LINE IS TO BE DRAWN.                                 *
C*    DASH          IS THE LENGTH OF THE DASH AND SPACE BETWEEN DASHES.  *
C*                                                                       *
C*    A DASHED LINE IS DRAWN IN INCHES FROM THE CURRENT PEN POSITION TO  *
C*    THE SPECIFIED XPAGE, YPAGE. THE SIZE OF THE DASH WILL BE AS CALLED *
C*    FOR EXCEPT IF THE LINE LENGTH IS LESS THAN DOUBLE THE DASH SIZE    *
C*    THE DASH IS ADJUSTED TO HALF THE LINE LENGTH.                      *
C*************************************************************************
      SUBROUTINE DASHP( X, Y, DL )
C
C     ***  DETERMINE CURRENT PEN POSITION  ***
C      
      CALL WHERE( XT, YT, ST )
C      
C     ***  COMPUTE DELTAX AND DELTAY  ***
C      
      DX = X - XT
      DY = Y - YT
      DS = DL
      IC = 2
C      
C     ***  DERIVE LINE LENGTH  ***
C      
      S = SQRT( DX*DX + DY*DY )
C      
C     ***  '83-2-28 BY ONUMA  ***
C      
      IF( S*ST .GE. 0.02 ) THEN
         DS = DS/S
C         
C     ***  TEST IF LINE LESS THAN DOUBLE DASH LENGTH  ***
C         
         IF( DS - 0.5 .GT. 0.0 ) THEN
            DS = 0.5
         ENDIF
C
         DX = DX*DS
         DY = DY*DS
C         
         S = DX
         ST = ABS(DX) - ABS(DY)
C         
         IF( ST .LT. 0.0 ) THEN
            S = DY
         ENDIF
C         
         ST = ABS(S/DS) - ABS(S)
         DS = ABS(S)
C
C     ***  DASHED LINE LOOP  ***
C
 3000    CONTINUE
         XT = XT + DX
         YT = YT + DY
         ST = ST - DS
         CALL PLOT( XT, YT, IC )
         IC = 5 - IC
C         
         IF( ST .GT. 0.0 ) THEN
            GO TO 3000
         ENDIF
      ENDIF
C      
C     ***  LAST SPECIFIC LINE SEGMENT CALL  ***
C      
      CALL PLOT( X, Y, IC )
C      
      RETURN
      END
