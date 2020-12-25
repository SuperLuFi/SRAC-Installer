      SUBROUTINE NLBIT(X, EXFUN, XL, XR, EPS, ITR, IR, IER)
************************************************************************
*  SOLUTION OF A NON-LINEAR EQUATION BY BISECTION METHOD AND REGULA    *
*     FALSI METHOD.                                                    *
*  PARAMETERS                                                          *
*    (1) X: VALUE OF THE SOLUTION                                      *
*    (2) EXFUN: THE FUNCTION OF THE NON-LINEAR EQUATION                *
*    (3) XL: LEFT HAND SIDE VALUE OF THE SOLUTION                      *
*    (4) XR: RIGHT HAND SIDE VALUE OF THE SOLUTION                     *
*    (5) EPS: TOLERANCE FOR CONVERGENCE                                *
*    (6) ITEND: MAXIMUM NUMBER OF REPITITION                           *
*    (7) IER: ERROR CODE                                               *
*  COPYRIGHT  T. OGUNI   JUNE 30 1989   VERSION 1.0                    *
C
C   MODIFIED BY M.SASAKI FOR SINGLE PRECISION SOLUTION. (JUN 1994)
C
************************************************************************
      IMPLICIT REAL*8(A-H,O-Z)
 
CJRI 21 JUN 1994
      REAL EXFUN
      EXTERNAL EXFUN
CJRI.END
C
      IER = 0
CKSK  EPS1 = 1.0D-10
      EPS1 = 1.0D-6
      X = XL
      FL = EXFUN(REAL(X))
      IF ( FL .EQ. 0.0) RETURN
      X = XR
      FR = EXFUN(REAL(X))
      IF (FR .EQ. 0.0) RETURN
      IF (FR * FL .GT. 0.0) THEN
       X = (XL * XR) * 0.5
       IER = 2
CKSK   WRITE(*,*) '(SUBR. NLBIT) NO CALUCULATION FOR F(XL)*F(XR) > 0.'
       WRITE(6,*) ' (SUBR. NLBIT) NO CALUCULATION FOR F(XL)*F(XR) > 0.'
       RETURN
      ENDIF
      DO 250 N=1,ITR
       X = (XL + XR) * 0.5
       F = EXFUN(REAL(X))
       IR = N
       IF (F .EQ. 0.0) RETURN
       IF (F * FL .LT. 0.0) THEN
        XR = X
        FR = F
       ELSE
        XL = X
        FL = F
       ENDIF
       DF = FL - FR
       DX = (XR - XL) * FL / DF
       X = XL + DX
       F = EXFUN(REAL(X))
       IF (F .EQ. 0.0) RETURN
       IF (FL * F .LT. 0.0) THEN
        XR = X
        FR = F
       ELSE
        XL = X
        FL = F
       ENDIF
       SX = XR - XL
       IF (DABS(X) .GT. 1.0) SX = SX / X
       IF (DABS(SX) .LE. EPS) THEN
        X = XR
        IF (DABS(FR) .LE. EPS1) RETURN
        X = XL
        IF (DABS(FL) .LE. EPS1) RETURN
       ENDIF
  250 CONTINUE
      IER = 1
CKSK  WRITE(*,*) '(SUBR. NLBIT) REQUIRED ACCURACY IS NOT OBTAINED.'
      WRITE(6,*) ' (SUBR. NLBIT) REQUIRED ACCURACY IS NOT OBTAINED.'
      RETURN
      END
