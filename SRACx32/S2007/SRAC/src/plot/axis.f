      SUBROUTINE AXIS( XPAGE, YPAGE, IBCD, NCHAR, AXLEG, ANGLE,
     1                 FIRSTV, DELTA, DIV )
C*************************************************************************
C*   MODIFIED FOR METRIC MEASURE  (CM-CENTIMETERS)                       *
C*         XPAGE,YPAGE  COORDINATES OF STARTING POINT OF AXIS, IN CM     *
C*         IBCD         AXIS TITLE.                                      *
C*         NCHAR        NUMBER OF CHARACTERS IN TITLE. + FOR C.C-W SIDE. *
C*         AXLEN        FLOATING POINT AXIS LENGTH IN CM                 *
C*         ANGLE        ANGLE OF AXIS FROM THE X-DIRECTION, IN DEGREES.  *
C*         FIRSTV       SCALE VALUE AT THE FIRST TIC MARK.               *
C*         DELTAV       CHANGE IN SCALE BETWEEN TIC MARKS ONE CM APART   *
C*      NOTE. LABELS WILL ONLY APPEAR ON EVERY OTHER TIC MARK            *
C*************************************************************************
      CHARACTER IBCD(*)
      COMMON /GCOMXY/ JFILM, KPEN, JOPT, KERR
      COMMON /GCOMXY/ FCT, HFCT, OLX, OLY, XRG, YRG, FF
C      
      FFC = 10.0/FCT
      DELTAV = DELTA*FFC
      KN = NCHAR
      A = 1.0
C      
      IF( KN .LT. 0 ) THEN
         A  = -A
         KN = -KN
      ENDIF
C      
      EX  = 0.0
      ADX = ABS(DELTAV)
C      
      IF( ADX .NE. 0.0 ) THEN
 3000    IF( ADX .GE. 99.0 ) THEN
            ADX = ADX*0.1
            EX  = EX + 1.0
            GO TO 3000
         ENDIF
C     
 3100    IF( ADX .LT. 0.01 ) THEN
            ADX = ADX*10.0
            EX  = EX - 1.0
            GO TO 3100
         ENDIF
      ENDIF
C      
      XVAL = FIRSTV*10.0**(-EX)
      ADX  = 2.0*DELTAV*10.0**(-EX)
      STH  = ANGLE*0.0174533
      CCTH = COS(STH)*FFC
      CTH  = CCTH/FFC*10.0/FCT
      SSTH = SIN(STH)*FFC
      STH  = SSTH/FFC*10.0/FCT
C      
      CTH2 = CCTH + CCTH
      STH2 = SSTH + SSTH
C      
      DXB = -0.254
      DYB = 0.38*A - 0.127
C      
      XN = XPAGE + DXB*CTH - DYB*STH
      YN = YPAGE + DYB*CTH + DXB*STH
C      
      AXLEN = AXLEG*0.1*FCT
      NTIC  = AXLEG/FFC + 1.0 + 0.00001
      NTC = ( NTIC + 1 )/2
      NT  = NTC/2
C      
      DO 4000 I = 1, NTC
         CALL NUMBER( XN, YN, 2.10/FCT, XVAL, ANGLE, 2 )
         IF( I .LE. 1 ) THEN
            XN = XPAGE + 2.0*DXB*CTH - DYB*STH
            YN = YPAGE + 2.0*DXB*STH + DYB*CTH
         ENDIF
         XVAL = XVAL + ADX
         XN   = XN + CTH2
         YN   = YN + STH2
C         
         IF( NT .EQ. 0 ) THEN
            Z = KN
            IF( EX .NE. 0.0 ) THEN
               Z = Z + 7.0
            ENDIF
C            
            DXB = -0.14*Z + AXLEN*0.5
            DYB = 0.825*A - 0.190
            XT = XPAGE + DXB*CTH - DYB*STH
            YT = YPAGE + DYB*CTH + DXB*STH
            CALL SYMBOL( XT, YT, 2.8/FCT , IBCD(1), ANGLE, KN )
C     
            IF( EX .NE. 0.0 ) THEN
               Z = KN + 2
               XT = XT + Z*CTH*0.28
               YT = YT + Z*STH*0.28
               CALL SYMBOL( XT, YT, 2.8/FCT, 3H*10, ANGLE, 3 )
               XT = XT + ( 3.0*CTH - 0.8*STH )*0.28
               YT = YT + ( 3.0*STH + 0.8*CTH )*0.28
               CALL NUMBER( XT, YT, 2.1/FCT, EX, ANGLE, -1 )
            ENDIF
         ENDIF
         NT = NT - 1
 4000 CONTINUE
C      
      CALL PLOT( XPAGE + AXLEN*CTH, YPAGE + AXLEN*STH, 3 )
      DXB = -0.178*A*STH
      DYB = +0.178*A*CTH
      A   = NTIC - 1
      XN = XPAGE + A*CCTH
      YN = YPAGE + A*SSTH
C      
      DO 4100 I = 1, NTIC
         CALL PLOT( XN, YN, 2 )
         CALL PLOT( XN + DXB, YN + DYB, 2 )
         CALL PLOT( XN, YN, 3 )
         XN = XN - CCTH
         YN = YN - SSTH
 4100 CONTINUE
C      
      RETURN
      END
