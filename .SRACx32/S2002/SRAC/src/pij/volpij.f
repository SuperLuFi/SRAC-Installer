      SUBROUTINE VOLPIJ(NREG,IRR,IXR,MMR,V,VOL,VOLRR,VOLX,VOLM)
      DIMENSION NREG(NZ),IRR(NR),IXR(NRR),MMR(NRR),V(NZ),VOL(NR)
     *         ,VOLRR(NRR),VOLX(NXR),VOLM(NM)
C
      COMMON / MAINC / DDM(63),NOUT1,NOUT2,DDM1(37),TITLE(18),DDM2(380)
      COMMON / PIJ2C / IGT,NZ,NR,NRR,NXR,IDUM6(27),NM
      CHARACTER*4 REG(5)
      DATA  REG/'(T)-','(R)-','(M)-','(X)-','(S)-'/
C
C     VOLUME OF REGION
C
      WRITE(NOUT2,3006) REG(5),(I,V(I),I=1,NZ)
      DO 84 J=1,NZ
      I=NREG(J)
      VOL(I)=VOL(I) + V(J)
   84 CONTINUE
      WRITE(NOUT2,3006) REG(1),(I,VOL(I),I=1,NR)
      IF(NR.EQ.NRR) GO TO 100
      DO 86 J=1,NR
      I=IRR(J)
      VOLRR(I)=VOLRR(I) + VOL(J)
   86 CONTINUE
      WRITE(NOUT2,3006) REG(2),(I,VOLRR(I),I=1,NRR)
  100 CONTINUE
      IF(NRR.EQ.NXR)GO TO 110
      DO 88 J=1,NRR
      I=IXR(J)
      IF(I.EQ.0) GO TO 88
      VOLX(I)=VOLX(I) + VOLRR(J)
   88 CONTINUE
      WRITE(NOUT2,3006) REG(4),(I,VOLX(I),I=1,NXR)
  110 CONTINUE
      IF(NRR.EQ.NM) GO TO 120
      DO 89 J=1,NRR
      I=MMR(J)
      VOLM(I)=VOLM(I)+VOLRR(J)
   89 CONTINUE
      WRITE(NOUT2,3006) REG(3),(I,VOLM(I),I=1,NM)
  120 CONTINUE
      DO 160 I=1,NR
      IF(V(I).LE.0) GO TO 9001
  160 CONTINUE
      TVL=0.
      DO 90 I=1,NR
      TVL=TVL+VOL(I)
   90 CONTINUE
      WRITE (NOUT2,3020) TVL
 3020 FORMAT (10X,17HTOTAL VOLUME     ,14X,1PE11.4)
C
      RETURN
 9001 WRITE(NOUT1,9103)I
      STOP
 9103 FORMAT(' **** UNDEFINED REGION  ',I3,' FOUND IN VOLPIJ' )
 3006 FORMAT(10X,10HVOLUME OF ,A4,6HREGION/(10X,I3,1H),1PE11.4,I3,
     *        1H),E11.4
     1 ,I3,1H),E11.4,I3,1H),E11.4,I3,1H),E11.4,I3,1H),E11.4,I3,1H),E11.4
     2,I3,1H),E11.4))
      END
