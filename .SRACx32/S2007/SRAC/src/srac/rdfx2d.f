      SUBROUTINE RDFX2D(DF,FLUX,MESH,IXM,IYM,IGM,IMESH,VOLUME,
     *                  NRGNE,PVOL,LMAX                        )
C
C     READ 2-DIMENSIONAL FLUX (CITATION)
C
CDEL  INTEGER RGX , MSX , ZNEX , ZDX , WZX
CDEL  PARAMETER ( RGX=100, MSX=211, ZDX=200, ZNEX=1000, WZX=100 )
      INCLUDE  'CITPMINC'
C
      REAL*8 DF
      DIMENSION DF(IXM,IYM),FLUX(IGM,IMESH),MESH(3,IMESH),VOLUME(IMESH),
     *          NRGNE(IXM,IYM),PVOL(LMAX)
C
C
      IBLSB = 1341 + 4*WZX
      IBMES = 33   + 6*RGX + 9*MSX + 2*ZNEX
      IBFLX = 76   + 2*MSX
      IBBUR = 530  + 11*ZDX
C
      LENG  = IBLSB + IBMES + IBFLX + IBBUR
     &      + 26    + 109   + 63    + 141 + 500
C
      REWIND 92
CMOD  READ(92) (DUM,I=1,8740),((NRGNE(I,J),I=1,IXM),J=1,IYM),
      READ(92) (DUM,I=1,LENG),((NRGNE(I,J),I=1,IXM),J=1,IYM),
     1        (PVOL(I),I=1,LMAX)
      REWIND 92
C
      LFLUX=9
      REWIND LFLUX
      DO 130 K=1,IGM
          READ(LFLUX) ((DF(I,J),I=1,IXM),J=1,IYM)
          DO 120 L=1,IMESH
              IX = MESH(1,L)
              IY = MESH(2,L)
              IF(IX.LT.1 .OR. IX.GT.IXM) GO TO 100
              IF(IY.LT.1 .OR. IY.GT.IYM) GO TO 100
              FLUX(K,L)=DF(IX,IY)
              GO TO 110
  100         CONTINUE
              FLUX(K,L)=-1.0E11
  110         CONTINUE
  120     CONTINUE
  130 CONTINUE
      DO 140 I = 1,IMESH
          VOLUME(I) = 0.0
          IX = MESH(1,I)
          IY = MESH(2,I)
          IF(IX.LT.1 .OR. IX.GT.IXM) GO TO 140
          IF(IY.LT.1 .OR. IY.GT.IYM) GO TO 140
          L = NRGNE(IX,IY)
          VOLUME(I) = PVOL(L)
  140 CONTINUE
      RETURN
      END
