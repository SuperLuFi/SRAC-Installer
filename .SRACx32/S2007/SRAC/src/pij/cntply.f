C             CNTPLY              LEVEL=1        DATE=84.03.06
C
C***********************************************************************
C                                                                      *
C       CNTPLY    : DRAWING POLYGON                                    *
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
C       GEOM04,GEOM05,GEOM06,GEOM07                                    *
C                                                                      *
C    -- CALLS --                                                       *
C                                                                      *
C       POLY                                                           *
C                                                                      *
C......................................................................*
C                                                                      *
C    -- INPUT --                                                       *
C                                                                      *
C       (ARGUMENT)                                                     *
C           N     : THE NUMBER OF THE SIDE.                            *
C           H     : THE LENGTH OF THE SIDE.                            *
C                                                                      *
C                                                                      *
C                                                                      *
C***********************************************************************
C                                                                      *
      SUBROUTINE CNTPLY(H,N)
      DATA  PAI/3.141592/
C
      X  = -H/2.
      TH = ((N-2)*PAI)/(2*N)
      Y  = -H/2.*TAN(TH)
      CALL POLY(X,Y,H,FLOAT(N),0.)
C
      RETURN
      END
