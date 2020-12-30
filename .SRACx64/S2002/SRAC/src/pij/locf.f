      FUNCTION LOCF(I,J,N,NT)
      IA=N-I
      JA=N-J
      II=MIN0(IA,JA)
      JJ=MAX0(IA,JA)
      LOCF=(JJ*(JJ+1))/2+II
      LOCF=NT-LOCF
      IF(II.LT.0) LOCF=0
      RETURN
      END
