C ********    ONE   ***********                  DATE=88.05.19
      SUBROUTINE ONE (U0,T1,ICOOD1,N)
      GO TO (1,2,3),ICOOD1
    1 T1 = ENX(N,U0)
      RETURN
    2 T1 = FKIN(N,U0)
      RETURN
    3 T1 = EXP(-U0)
      RETURN
    4 T1 = 0.
      RETURN
      END
