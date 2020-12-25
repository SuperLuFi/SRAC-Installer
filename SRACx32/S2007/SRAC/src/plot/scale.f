C*************************************************************************
C*    SUBROUTINE SCALE( ARRAY, AXLEN, NPTS, INC, DIM )                   *
C*                                                                       *
C*    MODIFIED FOR METRIC MEASURE (CM-CENTIMETERS) COMMENTS ONLY CHANGED *
C*         ARRAY   NAME OF ARRAY CONTAINING VALUES TO BE SCALED.         *
C*         AXLEN   LENGTH IN CM OVER WHICH ARRAY IS TO BE SCALED         *
C*         NPTS    NUMBER OF POINTS TO BE SCALED.                        *
C*         INC     INCREMENT OF LOCATION OF SUCCESSIVE POINTS.           *
C*************************************************************************
      SUBROUTINE SCALE( ARRAY, AXLEN, NPTS, INC, DIM )
C      
      REAL*4 ARRAY(*), SAVE(7)
C     
      SAVE(1) = 1.0
      SAVE(2) = 2.0
      SAVE(3) = 4.0
      SAVE(4) = 5.0
      SAVE(5) = 8.0
      SAVE(6) = 10.0
      SAVE(7) = 20.0
C      
      FAD = 0.01
      K   = IABS(INC)
      N   = NPTS*K
      Y0  = ARRAY(1)
      YN  = Y0
C      
      DO 4000 I = 1, N, K
         YS = ARRAY(I)
         IF( Y0 .GT. YS ) THEN
            Y0 = YS
         ELSE
            IF( YS .GT. YN ) THEN
               YN = YS
            ENDIF
         ENDIF
 4000 CONTINUE
C      
      FIRSTV = Y0
      IF( Y0 .LT. 0.0 ) THEN
         FAD = FAD - 1.0
      ENDIF
      DELTAV = ( YN - FIRSTV )/AXLEN
C      
      IF( DELTAV .LE. 0.0 ) THEN
         DELTAV = 2.0*FIRSTV
         DELTAV = ABS(DELTAV/AXLEN) + 1.0
      ENDIF
      
      I = ALOG10(DELTAV) + 1000.0
      P = 10.0**( I - 1000 )
      DELTAV = DELTAV/P - 0.01
C      
      DO 4100 I = 1, 6
         IS = I
         IF( SAVE(I) .GE. DELTAV ) THEN
            GO TO 3000
         ENDIF
 4100 CONTINUE
C      
 3000 CONTINUE
C      
      DELTAV = SAVE(IS)*P
      FIRSTV = DELTAV*AINT( Y0/DELTAV + FAD )
      T = FIRSTV + ( AXLEN + 0.01 )*DELTAV
      IF( T .LT. YN ) THEN
         FRISTV = P*AINT( Y0/P + FAD )
         T = FIRSTV + ( AXLEN + 0.01 )*DELTAV
         IF( T .LT. YN ) THEN
            IS = IS + 1
            GO TO 3000
         ENDIF
      ENDIF
C     
      FIRSTV = FIRSTV - AINT( ( AXLEN +
     1        ( FIRSTV - YN )/DELTAV )/2.0 )*DELTAV
C      
      IF( Y0*FIRSTV .LE. 0.0 ) THEN
         FIRSTV = 0.0
      ENDIF
C      
      IF( INC .LE. 0.0 ) THEN
         FIRSTV = FIRSTV + AINT( AXLEN + 0.5 )*DELTAV
         DELTAV = -DELTAV
      ENDIF
C      
      N = N + 1
      ARRAY(N) = FIRSTV
      N = N + K
      ARRAY(N) = DELTAV
C      
      RETURN
      END
