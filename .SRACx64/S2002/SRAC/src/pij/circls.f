C             CIRCLS              LEVEL=1        DATE=84.03.06
C
C***********************************************************************
C                                                                      *
C       CIRCLS    :   DRAWING CIRCLE  IN LIMIT SQUARE                  *
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
C     HLEN        : THE HALF LENGTH OF THE LIMIT SQUARE                *
C                                                                      *
C    -- LOCAL VARIABLES --                                             *
C                                                                      *
C      ANGS       : START POINT  OF ANGLE                              *
C      ANGE       : END   POINT  OF ANGLE                              *
C                                                                      *
C***********************************************************************
C                                                                      *
      SUBROUTINE CIRCLS ( XO,YO, R,  HLEN )
      DATA  PAI  /  3.14159265  /
C
C
      IF ( R .GT. HLEN )   GO TO 5
         CALL CIRCLG ( XO,YO, R, 0.0, 2*PAI )
         RETURN
    5 CONTINUE
         THS = ACOS(HLEN/R)
         THE = PAI/2 - THS
         DO  10  I = 1,4
            CALL  CIRCLG ( XO,YO, R, THS, THE )
            THS = THS + PAI/2
   10       THE = THE + PAI/2
         RETURN
      END
