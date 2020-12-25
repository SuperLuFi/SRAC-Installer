      SUBROUTINE RFLX3D(F,FLUX,MESH,IXM,IYM,IZM,IXYM,IGM,IMESH,VOLUME,
     *                  NRGNE,PVOL ,LVX                               )
C
C     READ 3-DIMENSIONAL FLUX (CITATION)
C
CDEL  INTEGER RGX , MSX , ZNEX , ZDX , WZX
CDEL  PARAMETER ( RGX=100, MSX=211, ZDX=200, ZNEX=1000, WZX=100 )
      INCLUDE  'CITPMINC'
C
      DIMENSION F(IXYM,IZM),FLUX(IGM,IMESH),MESH(3,IMESH),VOLUME(IMESH),
     *          NRGNE(IXM,IYM,IZM),PVOL(LVX)
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
CMOD  READ (92) (DUM,I=1,8740),(((NRGNE(J,I,KB),J=1,IXM),I=1,IYM),
      READ (92) (DUM,I=1,LENG),(((NRGNE(J,I,KB),J=1,IXM),I=1,IYM),
     1          KB=1,IZM),(PVOL(L),L=1,LVX)
      REWIND 92
C
      LFLUX=9
      REWIND LFLUX
      DO 100 K=1,IGM
          READ(LFLUX) ((F(I,J),I=1,IXYM),J=1,IZM)
          DO 200 L=1,IMESH
              IX =MESH(1,L)
              IY =MESH(2,L)
              IZ =MESH(3,L)
              IF(IX.LT.1 .OR. IX.GT.IXM) GO TO 300
              IF(IY.LT.1 .OR. IY.GT.IYM) GO TO 300
              IF(IZ.LT.1 .OR. IZ.GT.IZM) GO TO 300
              IPP=IXM*(IY-1)+IX
              FLUX(K,L)=F(IPP,IZ)
              GO TO 400
  300         CONTINUE
              FLUX(K,L)=-1.0E11
  400         CONTINUE
  200     CONTINUE
  100 CONTINUE
      DO 500 I = 1,IMESH
          VOLUME(I) = 0.0
          IX = MESH(1,I)
          IY = MESH(2,I)
          IZ = MESH(3,I)
          IF(IX.LT.1 .OR. IX.GT.IXM) GO TO 500
          IF(IY.LT.1 .OR. IY.GT.IYM) GO TO 500
          IF(IZ.LT.1 .OR. IZ.GT.IZM) GO TO 500
          L = NRGNE(IX,IY,IZ)
          VOLUME(I) = PVOL(L)
  500 CONTINUE
      RETURN
      END
