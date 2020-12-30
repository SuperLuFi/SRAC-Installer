C*************************************************************************
C*    SUBROUTINE     POLY                                                *
C*            04 CHAR   PLOT      EBCDIC    MM + CM + INCH               *
C*************************************************************************
      SUBROUTINE POLY( X, Y, SL, RN, TH )
C      
      N   = RN
      XN  = X
      YN  = Y
      THO = TH*0.01745
      CALL PLOTGM( X, Y, 3 )
C      
      IF( N .LT. 0 ) THEN
         TH1 = -6.2832/RN
         TH2 = -TH1*2.0
         N   = -N
         DO 4000 I = 1, N
            XN = XN + SL*COS(THO)
            YN = YN + SL*SIN(THO)
            CALL PLOTGM( XN, YN, 2 )
            THO = THO + TH1
            XN = XN + SL*COS(THO)
            YN = YN + SL*SIN(THO)
            CALL PLOTGM( XN, YN, 2 )
            THO = THO + TH2
 4000    CONTINUE
      ELSEIF( N .GT. 0 ) THEN
         TH1 = 6.2832/RN
         DO 4100 I = 1, N
            XN = XN + SL*COS(THO)
            YN = YN + SL*SIN(THO)
            CALL PLOTGM( XN, YN, 2 )
            THO = THO + TH1
 4100    CONTINUE
      ENDIF
C      
      RETURN
      END
