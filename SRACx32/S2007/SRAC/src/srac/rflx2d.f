      SUBROUTINE RFLX2D(F     ,FLUX  ,MESH ,IXM   ,IYM   ,IGMAX ,IMESH ,
     *                  VOLUME,IVMESH,VOL                              )
C
C     READ 2-DIMENSIONAL FLUX (TWOTRAN)
C
      DIMENSION F(IXM,IYM),FLUX(IGMAX,IMESH),MESH(3,IMESH),VOLUME(IMESH)
     *         ,VOL(IXM,IYM)
C
      REWIND IVMESH
      READ (IVMESH) ((VOL(I,J),I=1,IXM),J=1,IYM)
      REWIND IVMESH
      DO 500 L = 1,IMESH
      IX = MESH(1,L)
      IY = MESH(2,L)
      VOLUME(L) = 0.0
      IF (IX.LT.1 .OR. IX.GT.IXM) GO TO 500
      IF (IY.LT.1 .OR. IY.GT.IYM) GO TO 500
      VOLUME(L) = VOL(IX,IY)
  500 CONTINUE
      LFLUX=33
      REWIND LFLUX
      DO 100 K=1,IGMAX
          READ(LFLUX) ((F(I,J),I=1,IXM),J=1,IYM)
          DO 200 L=1,IMESH
              IX =MESH(1,L)
              IY =MESH(2,L)
              IF(IX.LT.1 .OR. IX.GT.IXM) GO TO 300
              IF(IY.LT.1 .OR. IY.GT.IYM) GO TO 300
              FLUX(K,L)=F(IX,IY)
              GO TO 400
  300         CONTINUE
              FLUX(K,L)=-1.0E11
  400         CONTINUE
  200     CONTINUE
  100 CONTINUE
      RETURN
      END
