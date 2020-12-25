      SUBROUTINE VINT(NRMAX,NK,RN,TAU,RRN,ANS,NNMAX1)
C
      DIMENSION NK(NRMAX), RN(NRMAX) , TAU(NNMAX1,8) , RRN(NNMAX1)
C
      ANS      =  0.0
      NN1      =  0
      NNR      = -1
      DO 100 K = 1,NRMAX
      RN(K)    = 0.0
      NNR      = NNR + 1
      NC       = NK(K)
      DO 99 N  = 1,NC
      NN1      = NN1 + 1
      NNR      = NNR + 1
   99 RN(K)    = RN(K) + RRN(NNR)*TAU(NN1,4) + RRN(NNR+1)*TAU(NN1+1,3)
  100 ANS      = ANS   + RN(K)
      RETURN
C     DEBUG SUBCHK
      END
