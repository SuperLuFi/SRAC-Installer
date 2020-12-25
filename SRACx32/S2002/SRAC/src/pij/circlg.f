C             CIRCLG              LEVEL=1        DATE=84.03.06
C
C***********************************************************************
C                                                                      *
C       CIRCLG    :   DRAWING CIRCLE SUBROUTINE                        *
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
C         CIRCL                                                        *
C                                                                      *
C......................................................................*
C                                                                      *
C    -- INPUT --                                                       *
C                                                                      *
C       (ARGUMENT)                                                     *
C     XO,YO       : CENTER POSITION                                    *
C     R           : THE RADIUS OF THE DESIRED CIRCLE IN (MM).          *
C     THS         : THE ANGLE OF THE START POINT                       *
C     THE         : THE ANGLE OF THE END POINT                         *
C                                                                      *
C                                                                      *
C    -- LOCAL VARIABLES --                                             *
C                                                                      *
C      X,Y        : START POINT                                        *
C                                                                      *
C***********************************************************************
C                                                                      *
      SUBROUTINE CIRCLG ( XO,YO, R, THS, THE )
      DATA  PAI  /  3.14159265  /
C
      X = XO + R*COS(THS)
      Y = YO + R*SIN(THS)
      CALL   CIRCL  ( X, Y, THS*180/PAI, THE*180/PAI, R, R, 0.0 )
C
      RETURN
      END
