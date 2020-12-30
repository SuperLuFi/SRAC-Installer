      SUBROUTINE RTFX1D(F,FLUX,MESH,IXM,IGM,IMESH,IXM1,VOLUME,VOL )
C
C     READ 1-DIMENSIONAL FLUX (TUD)
C
      COMMON /TUD1C/  NR,NOU1(5),IG,NOU2(23),LCNK,DUM,LCRX,NOUSE3(17),
     1                RADI(500)
      DIMENSION F(IXM1,IGM),FLUX(IGM,IMESH),MESH(3,IMESH),NK(500),
     *          VOLUME(IMESH),VOL(IXM)
      EQUIVALENCE (NK(1),RADI(1))
C
      LFLUX=33
      REWIND LFLUX
      READ(LFLUX) ((F(I,J),I=1,IXM1),J=1,IGM)
      IR       = 0
      IMESHR   = NK(LCNK)+1
      DRAD     = RADI(LCRX)/NK(LCNK)
      RAD      = 0.0
      RADP     = 0.0
      DO 120 I = 2,IXM+1
          IF (I.LE.IMESHR) GO TO 100
          IR     = IR + 1
          IMESHR = IMESHR + NK(LCNK+IR)
          DRAD   = RADI(LCRX+IR) / NK(LCNK+IR)
  100     CONTINUE
          RAD   = RAD + DRAD
          IG1   = IG + 1
          GO TO (101,102,103) , IG1
  101     VOL(I-1) = DRAD
          GO TO 120
  102     VOL(I-1) = 3.141593*(RAD*RAD-RADP*RADP)
          GO TO 115
  103     VOL(I-1) = 4.188790*(RAD**3-RADP**3)
C----- SPHERE GEOMETORY
          DO 110 J = 1,IGM
              F(I,J)=F(I,J)/RAD
  110     CONTINUE
  115 CONTINUE
      RADP = RAD
  120 CONTINUE
  130 CONTINUE
      DO 180 J=1,IMESH
          IX =MESH(1,J)
          VOLUME(J)=0.0
          IF(IX.LT.1 .OR. IX.GT.IXM) GO TO 150
          VOLUME(J) = VOL(IX)
          DO 140 I=1,IGM
              FLUX(I,J)=(F(IX,I)+F(IX+1,I))/2.0
  140     CONTINUE
          GO TO 170
  150     CONTINUE
          DO 160 I=1,IGM
              FLUX(I,J)=-1.0E11
  160     CONTINUE
  170     CONTINUE
  180 CONTINUE
      RETURN
      END
