      FUNCTION   FKIN(N,X)
      COMMON /ABC11/ A(5500),B(5500),C(5500)
C BICKLEY-NAYLOR FUNCTION BY RATIONAL APPROX PARAMETER GIVEN BY MAKINO
C NUCLEONIK 9 BAND 7 HEFT 1967
C X>6.0 RANGE RESTRICTED TO BE OVERFLOW FROM TABULATED ONES
C N=1,2,3  ONLY
      DIMENSION AA(4,2,3),BB(5,2,3),CC(2,3),PX(2,3)
      REAL *8          AA,BB,XX,QQ,PP ,A,B,C
      DATA AA/
     1  .96572105   , -.28959120   , .030610238  , -.0011370233 ,
     2  .36668658D-2, -.82618978D-3, .63930345D-4, -.16954459D-5,
     3  .43228799   , -.12254097   , .012167113  , -.42191691D-3,
     4  .16388649D-2, -.35676160D-3, .26622503D-4, -.67972543D-6,
     5  .084790066  , -.022044221  , .0019904763 , -.62257550D-4,
     6  .32297971D-3, -.65260999D-4, .45013738D-5, -.10581271D-6/
      DATA BB/
     1  1.0,  .45118240  , .73261267   , -.039791833  , .039573198  ,
     2  1.0, -.78064825  , .24422565   , -.035315751  , .0023728056 ,
     3  1.0, -.33898117  , .67917163   , -.097397491  , .029176632  ,
     4  1.0, -.72078149  , .20596925   , -.027412106  , .0016314771 ,
     5  1.0, -.91375070  , .50917132   , -.093411994  , .012892154  ,
     6  1.0, -.62018356  , .15021226   , -.016967420  , .80823019D-3/
      DATA CC/
     1    -0.625, .80368280,
     2    -1.125, 2.0285838,
     3    -1.625, 3.7538403/
      DATA PX/
     1  5.9, 9.4 , 6.5, 9.8, 7.5, 11.0/
C
      NN=INT(100.*X)+1
      DX=X-FLOAT(NN)*0.01
      IF(NN.GT.1100) GO TO 99
      NN=NN+ 1100*(N-1)
      FKIN =(B(NN)*DX+A(NN))*DX+C(NN)
      RETURN
   99 IF(N.GT.3) THEN
                 FKIN=0.
                 RETURN
                 ENDIF
      XX=X
      DO 10 I=1,2
      J=I
      IF(X .LT. PX(I,N)) GO TO 11
   10 CONTINUE
      FKIN=1.2533141*EXP(-X)/SQRT(X)*(1.0+(CC(1,N)+CC(2,N)/X)/X)
      GO TO 14
   11 QQ=AA(4,J,N)
      DO 12 MM=3,1,-1
   12 QQ=QQ*XX+AA(MM,J,N)
      PP=BB(5,J,N)
      DO 13 MM=4,1,-1
   13 PP=PP*XX+BB(MM,J,N)
      FKIN=QQ/PP
   14 RETURN
      END