C             CIRCLH              LEVEL=1        DATE=84.03.06
C
C***********************************************************************
C                                                                      *
C       CIRCLH    :   DRAWING CIRCLE  IN LIMIT HEXAGON                 *
C                                                                      *
C                                                                      *
C       MODULE NO.  :                                                  *
C       MODULE TYPE : SUBROUTINE                                       *
C       CODER       : HATA KENICHROU                                   *
C       DATE        : 1982.01.20                                       *
C                                                                      *
C***********************************************************************
C                                                                      *
C    -- CALLED BY --                                                   *
C                                                                      *
C         GEOMTY                                                       *
C                                                                      *
C    -- CALLS --                                                       *
C                                                                      *
C         CIRCLG                                                       *
C                                                                      *
C......................................................................*
C                                                                      *
C    -- INPUT --                                                       *
C                                                                      *
C       (ARGUMENT)                                                     *
C     XO YO       : THE CENTER POINT OF THE DESIRED CIRCLE.            *
C     R           : THE RADIUS OF THE DESIRED CIRCLE IN (MM).          *
C     THS         : THE ANGLE OF THE START POINT                       *
C     THE         : THE ANGLE OF THE END POINT                         *
C     HLEN        : THE SIDE LENGTH OF THE LIMIT HEXAGON               *
C                                                                      *
C    -- LOCAL VARIABLES --                                             *
C                                                                      *
C      ANGS       : START POINT  OF ANGLE                              *
C      ANGE       : END   POINT  OF ANGLE                              *
C                                                                      *
C***********************************************************************
C                                                                      *
      SUBROUTINE CIRCLH ( XO,YO, R,  HLEN )
      DATA  PAI  /  3.14159265  /
C
C
      IF ( R .GT. HLEN*SQRT(3/4.) )   GO TO 5
         CALL CIRCLG ( XO,YO, R, 0.0, PAI*2 )
         RETURN
    5 CONTINUE
         S = (R/HLEN)**2
         ANGE = ATAN( (SQRT(3.0)*(1-SQRT(4*S-3)))/(3+SQRT(4*S-3)) )
         ANGS = -ANGE
         DO  10  I = 1,6
            CALL  CIRCLG ( XO,YO, R, ANGS, ANGE )
            ANGS = ANGS + PAI/3
   10       ANGE = ANGE + PAI/3
         RETURN
      END
