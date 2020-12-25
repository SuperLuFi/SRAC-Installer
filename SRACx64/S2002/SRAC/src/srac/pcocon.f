      SUBROUTINE  PCOCON(AMASS,SIGC0,ICAPT,IFISS,NEF,IRES,LTOT,IDENT)
C
      COMMON /PCOWK3/ AA(14500),LOCAM(8),IFS
C
      DIMENSION       A(41),IA(41)
      CHARACTER*8     IDENT
C
      EQUIVALENCE    (IA(1),A(1))
C
C     READ CZMMXXXN MEMBER FROM FASTU OR MICREF LIBRARY
C
      IDENT (1:1) = 'C'
      CALL  READ(IDENT,IA,41)
C
      ICAPT = IA(1)
      IFISS = IA(2)
      IRES  = IA(3)
      LTOT  = IA(4)
      IFS   = IA(17)
      AMASS =  A(27)
      SIGC0 =  A(29)
C
      CALL  ICLEA(LOCAM, 8,0)
C
      IF(LTOT.LE.0)  RETURN
C
      IF(ICAPT.EQ.1)  LOCAM(1)=1
      IF(IFISS.GT.0)  THEN
                      LOCAM(2) = 1        + NEF*ICAPT
                      LOCAM(3) = LOCAM(2) + NEF
                      LOCAM(4) = LOCAM(3) + NEF
                      ENDIF
C
      LOCAM(5) = LOCAM(4) + NEF
      IF(IFISS.EQ.0)  LOCAM(5) = LOCAM(1) + NEF
      IF(ICAPT.EQ.0.AND.IFISS.EQ.0)  LOCAM(5)=1
      LOCAM(6) = LOCAM(5) + NEF
      LOCAM(7) = LOCAM(6) + NEF
      LOCAM(8) = LOCAM(7) + NEF
C
      RETURN
      END
