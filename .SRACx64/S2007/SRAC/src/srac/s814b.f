C             S814B               LEVEL=3        DATE=83.08.24
      SUBROUTINE S814B (V,AA,IGE,IP,IM,R,VOLM,MA,MZ)
C  *** S814B  COMPUTES AREAS AND VOLUMES FOR INITIAL PRINT
C
      DIMENSION V(IM),AA(IP),AF(3),VF(3),R(IP)
     &         ,VOLM(1),MA(1),MZ(1)
C
      AF(1) = 1.0
      AF(2) = 6.2831854
      AF(3) = 12.566371
      VF(1) = 1.0
      VF(2) = 0.5
      VF(3) = 0.3333333
      M = IGE - 1
      N = (IGE + 1) / IGE
C     N PREVENTS ERROR TRACE FOR 0**0 WHEN IGE=1 AND R(1)=0
      DO 16 I=N,IP
   16 AA(I) = AF(IGE) * R(I) ** M
      IF (IGE.EQ.1) AA(1) = AF(1)
      DO 17 I=1,IM
      V(I) = VF(IGE) * (AA(I+1)*R(I+1) - AA(I)*R(I))
      J = MA(I)
      JJ = IABS(MZ(J))
      VOLM(JJ) = VOLM(JJ) + V(I)
   17 CONTINUE
      RETURN
      END
