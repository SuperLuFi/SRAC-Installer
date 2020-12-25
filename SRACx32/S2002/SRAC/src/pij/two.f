C             TWO                 LEVEL=3        DATE=82.09.02
      SUBROUTINE TWO (U0,UI, T1,T2,ICOOD1,N)
      IF(U0.GT.7.) GO TO 4
      GO TO (1,2,3),ICOOD1
    1 T1 = ENX(N,U0)
      IF(U0+UI.GT.7.) GO TO 5
      T2 = ENX(N,U0+UI)
      RETURN
    2 T1 = FKIN(N,U0)
      IF(U0+UI.GT.7.) GO TO 5
      T2 = FKIN(N,U0+UI)
      RETURN
    3 T1 = EXP(-U0)
      IF(U0+UI.GT.7.) GO TO 5
      T2 = EXP(-(U0+UI))
      RETURN
    4 T1 = 0.
    5 T2 = 0.
      RETURN
      END
