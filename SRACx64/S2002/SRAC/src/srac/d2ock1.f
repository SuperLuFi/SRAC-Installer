      SUBROUTINE D2OCK1(NMAT,NM,ID2O,NISO,LISO,IDENT,MATD)
C
      COMMON /PDSPDS/ BUF(540),IFLSW,FILENM,ECODE,TEMPRY
C
      CHARACTER*4 IDENT(2,1),IDENT1
      CHARACTER*12 FILENM
C
      DIMENSION NISO(NMAT),MATD(NM),LISO(NMAT)
C
C
C
      IDENT1(1:1) = ' '
      ID2O        = 0
      DO 120 I = 1,NM
      MAT = IABS(MATD(I))
      IF(MAT.GT.NMAT) GO TO 150
C
      IF(NISO(MAT).GT.0) THEN
                         LOC         = LISO(MAT)
                         DO    110 J = 1,NISO(MAT)
                         IDENT1(2:4) = IDENT(1,LOC)(2:4)
                         LOC         = LOC + 1
                         IF (IDENT1.EQ.' D02') GO TO 130
  110                    CONTINUE
                         ENDIF
  120 CONTINUE
      RETURN
C
  130 CONTINUE
      ID2O = 1
C===== WHETHER OR NOT USER FILE DOES INCLUDE MEMBER 'YD020000' ?
      FILENM = 'FASTU       '
      CALL SEARCH('YD020000',LENGD,ISW)
      IF (ISW .EQ. 1) ID2O = 0
      RETURN
C
  150 WRITE(6,160) MAT,NMAT
      STOP
C
  160 FORMAT(' *** MATERIAL NUMBER  USED IN THE GEOMETRY ROUTINE ', I4,
     *' NOT FOUND IN ',I4,' MATERIALS  SPECIFIED ON II.8')
      END
