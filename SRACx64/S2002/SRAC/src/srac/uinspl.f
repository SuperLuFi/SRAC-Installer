      SUBROUTINE UINSPL(X,Y,DY,N,C,D,E,ICON)
C
C     UINSPL OBTAINS CUBIC SPLINE FUNCTIONS FROM GIVEN DATA POINTS,
C     (XI),Y(I),I=1,N)
C
C     X(INPUT)    : X1,X2,X3,...........,XN
C     Y(INPUT)    : Y1,Y2,Y3,...........,YN
C     DY(INPUT)   : DY(1)=Y1", DY(2)=YN" :2ND ORDER DERIVATIVE OF Y
C                   AT THE BOTH ENDS
C     N(INPUT)    : NUMBER OF POINTS (N>1)
C     C(OUTPUT)   : C1,C2,C3,...........,CN :COEFFICIENTS OF LINER TERM
C     D(OUTPUT)   : D1,D2,D3,...........,DN :COEFFICIENTS OF SQUARE TERM
C     E(OUTPUT)   : E1,E2,E3,...........,EN :COEFFICIENTS OF CUBIC TERM
C     ICON(OUTPUT): CONDITION CODE
C                   ICON=0: NORMAL END
C                   ICON=30000: N<2, OR X(I).GE.X(I+1)
C     LN1         : SIZE OF ARRAY X,Y
C     LN2         : SIZE OF ARRAY C,D,E
C
C     S(X) = Y1 + C1(X-X1) + D1(X-X1)**2               (X<X1)
C          = YI + CI(X-XI) + DI(X-XI)**2 + EI(X-XI)**3 (XI<X=<XI+1)
C          = YN + CN(X-XN) + DN(X-XN)**2               (X>XN)
C
      DIMENSION     X(*),Y(*),DY(*),C(*),D(*),E(*)
C
C     CHECK PARAMETERS
C
 1000 CONTINUE
      IF( N .LT. 2)     GO TO 9000
      N1 = N - 1
      DO  10  I = 1, N1
        IF( X(I) .GE. X(I+1) )     GO TO 9000
   10 CONTINUE
C
C     SET THE INITIAL VALUE
C
 1100 CONTINUE
      ICON = 0
      D(1) = DY(1)
      D(N) = DY(2)
      IF( N .EQ. 2 )     GO TO 1300
      H1   = X(2) - X(1)
      YH1  = ( Y(2) - Y(1) ) / H1
      H    = X(3) - X(2)
      YH   = ( Y(3) - Y(2) ) / H
      D(2) = 0.5 / (H1 + H )
      C(2) = 6.0 * ( YH -YH1 ) - H1 * D(1)
C
C         SOLVE THE SYSTEM OF LINEAR EQUATIONS
C         FOR THE SECOND DERIVATIVES BY THE
C         CHOLESKI'S METHOD
C
 1200 CONTINUE
      DO  30  I = 3, N1
        H1   = H
        YH1  = YH
        H    = X(I+1) - X(I)
        YH   = ( Y(I+1) - Y(I) ) / H
        TM   = H1 * D(I-1)
        D(I) = 1.0 / (2.0 * (H1 + H) -H1 * TM )
        E(I) = TM
        C(I) = 6.0 * (YH - YH1 ) - TM * C(I-1)
   30 CONTINUE
      D(N1) = ( C(N1) - H * D(N) ) * D(N1)
      N3    = N1 - 2
      DO  40  I = 1, N3
        I0  = N1 - I
        I1  = N  - I
        D(I0) = C(I0) * D(I0) - D(I1) * E(I1)
   40 CONTINUE
C
C     SOLVE THE COEFFICIENTS OF THE
C     CUBIC SPLINE INTERPOLATION FORMULAS
C
 1300 CONTINUE
      DO  50  I = 1, N1
        DW   = D(I)
        DW1  = D(I+1)
        H    = X(I+1) - X(I)
        C(I) = ( Y(I+1) - Y(I) ) / H
     *         - H * ( DW1 + 2.0 * DW ) / 6.0
        E(I) = ( DW1 - DW ) / (H * 6.0 )
        D(I) = DW * 0.5
   50 CONTINUE
      C(N)  = ( Y(N) - Y(N1) ) / H + H * ( D(N) + D(N1) ) / 3.0
      D(N)  = D(N) * 0.5
      E(N)  = 0.0
C
C     END PROCESS
C
C
 8000 CONTINUE
      RETURN
C
C     ERROR PROCESS
C
C
C     ---PARAMETER ERROR             ICON = 30000
 9000 CONTINUE
      ICON = 30000
      GO TO  8000
      END
