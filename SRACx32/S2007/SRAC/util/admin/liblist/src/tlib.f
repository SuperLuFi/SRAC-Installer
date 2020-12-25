      SUBROUTINE   TLIB(IDENT,LOUT,DIRNAM,NGT,ISWFT,NT,LORD,IOUT,IPRN)
C
      CHARACTER       DIRNAM*120,MEMBER*8
      CHARACTER*4     IDENT(2)
      COMMON  /WORK / A(20000)
CKSK  INTEGER*2       NP1(7)
      DIMENSION       NP1(7)
      DIMENSION       IA(20000)
      DIMENSION       TEMP(12)
      EQUIVALENCE     (A(1),IA(1))
C
C  ** SRAT OF PROCESS
C
      CALL  CLEA(  100,  A   , 0.0 )
      CALL  CLEA(   12, TEMP , 0.0 )
C
      MEMBER = IDENT(1)//IDENT(2)
      MEMBER(1:1) = 'C'
      MEMBER(8:8) = '0'
      LENG = 0
      CALL PDSIN(DIRNAM,MEMBER,IA,LENG,IOUT,IPRN,IRC)
      CALL PDSER(' TLIB ',DIRNAM,MEMBER,IRC,IOUT)
C
      IFISS  = IA(1)
      INP    = IA(2)
      ISWFT  = IA(3)
      NGMIN  = IA(4)
      NGMAX  = IA(5)
      NSIG   = IA(6)
      NT     = IA(7)
      XNU    =  A(29)
      LENGK  = IA(30)
CMOD  ISW    = 10
      ISW    =  8
      DO  45 I = 1 , NT
      TEMP(I)  = A(ISW+I)
      IF(I.LE.NT.AND.TEMP(I).LE.0.0) THEN
        WRITE(LOUT,55) MEMBER
      ENDIF
   45 CONTINUE
   55 FORMAT(5X,'MEMBER ',A8,' HAS ZERO TEMPERATURE ] ')
C
C-----SET NP1
      IKK      = INP
      DO 125 JJ= 7 , 2 , -1
      NDIV     = 2**(JJ-1)
      NP1(JJ)  = IKK/NDIV
      IF(NP1(JJ).EQ.1)  IKK = IKK - NDIV
  125 CONTINUE
      NP1(1)   = IKK
C     IPN=0 IS ASSUMED AS IPN=2 TO READ K-MATRIX
      IF(IKK.EQ.0.AND.NP1(2).EQ.0)  NP1(2) = 1
C
      LORD = 0
      IF(NP1(1).EQ.1)  LORD = 1
      IF(NP1(2).EQ.1)  LORD = 1
      IF(NP1(3).EQ.1)  LORD = 2
      IF(NP1(4).EQ.1)  LORD = 3
      IF(NP1(5).EQ.1)  LORD = 4
      IF(NP1(6).EQ.1)  LORD = 5
      IF(NP1(7).EQ.1)  LORD = 6
C
      MEMBER = IDENT(1)//IDENT(2)
      MEMBER(1:1) = 'K'
      MEMBER(8:8) = '0'
      LENG = -1
      CALL PDSIN(DIRNAM,MEMBER,IA,LENG,IOUT,IPRN,IRC)
      CALL PDSER(' TLIB ',DIRNAM,MEMBER,IRC,IOUT)
C
CM    IF(LENG.LT.NGT*NGT)  LORD = 0
      IF(LENGK.LT.NGT*NGT)  LORD = 0
C
      WRITE(6,*) ' MEMBER LENGK LENG NT TEMP(1) : ',MEMBER,LENGK,LENG,
     +                                             TEMP(1)
C
      RETURN
      END
