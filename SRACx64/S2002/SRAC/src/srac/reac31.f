      SUBROUTINE REAC31(MTNAME,NISO  ,IDENT ,IXMC  ,DN    ,
     1                  DN58  ,MPOSI ,LU235 ,LU238 ,NMAT  ,
     2                  NTISO ,NOUT2 ,JERR  ,NNAME         )
C
C     DENSITY OF LU235 AND LU238
C
      DIMENSION     NISO(NMAT),IXMC(NTISO),DN(NTISO),DN58(2)
C
      CHARACTER*4   IDENT(2,NTISO),NNAME(2)
      CHARACTER*8   MTNAME(NMAT)
C
C
C
      NT       = 0
      MT       = 0
      JERR     = 0
CMOD
C     DO 110 I = 1,NMAT
C     IF (NISO(I).GT.0) THEN
C                       MT   = MT + 1
C                       IF(MT.EQ.MPOSI) GO TO 120
C                       NT   = NT + NISO(I)
C                       ENDIF
C 110 CONTINUE
C     JERR    = 5
C     RETURN
      IF(MPOSI.GT.0.AND.MPOSI.LE.NMAT) THEN
                        DO 110 I = 1,MPOSI-1
      IF (NISO(I).GT.0) NT   = NT + NISO(I)
  110                   CONTINUE
                        MT      = MPOSI
                        GO TO 120
C
                        ELSE
                        MT      = 0
                        JERR    = 5
                        RETURN
                        ENDIF
C
  120 CONTINUE
      NNAME(1)   = IDENT(1,LU235+NT)
      DN58 (1)   = DN   (  LU235+NT)
      NNAME(2)   = IDENT(1,LU238+NT)
      DN58 (2)   = DN   (  LU238+NT)
C
      RETURN
      END
