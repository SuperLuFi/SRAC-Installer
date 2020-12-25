      FUNCTION  TITET(I)
CKSK  CALL  CLOCKM(K)
      CALL  UCLCKM(K)
      S = K
      IF(I.EQ.0) T1=S
      TITET = S - T1
      RETURN
      END
