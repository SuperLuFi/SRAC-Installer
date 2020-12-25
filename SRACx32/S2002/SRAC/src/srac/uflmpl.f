      SUBROUTINE   UFLMPL(IDENT ,AA    ,WF    ,NREG  ,BB    ,WR    ,
     &                    IPL   ,LAF   ,LDF   ,NEG   ,NETL  ,NEFL4 ,
     &                    LAB   ,LDB   ,NEF   ,NET )
C    *****************************************************************
C    *  THIS ROUTINE CLLAPSES MATICIES OF ANISOPTROPIC DATA WHICH IS *
C    *  THE COEFFICINT OF LEGENDRE FUNCTION EXPANSION.               *
C    *                   IDEN           MEANING                      *
C    *                  QZZM0000 ---->  P2 COMPONENT                 *
C    *                  SZZM0000 ---->  P3 COMPONENT                 *
C    *                  TZZM0000 ---->  P4 COMPONENT                 *
C    *                  UZZM0000 ---->  P5 COMPONENT                 *
C    *****************************************************************
      COMMON /PDSPDS/ BUFFER(540),IFLSW,NFILE(3),ECODE,ITMP
C
      DIMENSION   IDENT(2),AA(17000),WF(74),NREG(107),BB(107,74),
     &            WR(74),IDMPL(5)
C
      CHARACTER*4 IDMPL,NFILE
      DATA        IDMPL /'    ' ,'   Q' ,'   S' ,'   T' ,'   U' /
C
C---- INITIAL SET
C
      IF(IPL.LE.1)  RETURN
      IF(LAF.LE.0.OR.LAB.LE.0)  RETURN
      IF(IPL.GT.5)  IPL = 5
C
C     WRITE(6,101) IDENT
C     WRITE(6,102) IPL,LAF,LDF,NEG,NETL,NEFL4,LAB,LDB
C     WRITE(6,104) WF
C     WRITE(6,105) WR
C 101 FORMAT(/1H ,' ## IDENT(UFLMPL) ## ',2A4)
C 102 FORMAT(1H ,5X,' IPL -------- ',I6,
C    *      /1H ,5X,' LAF -------- ',I6,
C    *      /1H ,5X,' LDF -------- ',I6,
C    *      /1H ,5X,' NEG -------- ',I6,
C    *      /1H ,5X,' NETL-------- ',I6,
C    *      /1H ,5X,' NEFL4------- ',I6,
C    *      /1H ,5X,' LAB -------- ',I6,
C    *      /1H ,5X,' LDB -------- ',I6,/)
C 104 FORMAT(1H ,' ## WF ## ',1P10E11.4)
C 105 FORMAT(1H ,' ## WR ## ',1P10E11.4)
C
C     LOOP OF THE ORDER OF LGENDRE EXPANSION
C
      DO 1000 NN = 2,IPL
      NFILE(1) = 'FAST'
      NFILE(2) = 'P   '
      NFILE(3) = '    '
      CALL PACK(IDENT(1),1,IDMPL(NN))
      CALL SEARCH(IDENT(1),LENG,ISW)
      IF(ISW.EQ.1)  GO TO 1000
C
      CALL  CLEA(AA,17000,0.0)
      CALL  CLEA(BB, 7918,0.0)
      CALL  READ(IDENT,AA,LENG)
C
      LTHR =0
      LOC  =1
      LLEXT=LAF
      IF(NETL.EQ.0.AND.LLEXT.GT.NEFL4) LLEXT=NEFL4
      DO 700 N=1,LLEXT
      NJ=NREG(N)
      DO 650 K=N,N+LDF
      SAVE=AA(LOC)
      LOC=LOC+1
      IF(SAVE.EQ.0.0)  GO TO 650
      KK=K
      IF(K.GT.NEG) KK=NEG
      NK=NREG(KK)
      BB(NK,NJ)=BB(NK,NJ)+SAVE*WF(N)
  650 CONTINUE
      LTHR=MAX0(LTHR,NK-NJ)
  700 CONTINUE
C
C     BB TO AA
C
      IF(NJ.GT.NEF) NJ=NEF
      LA  = NJ
      LD  = LTHR
      LOC = 0
C     WRITE(6,7004) NJ,LTHR
C7004 FORMAT(1H ,' ## LA LD ## ',2I6)
      DO 800 NJ=1,LAB
      DO 750 NK=NJ,NJ+LDB
      LOC=LOC+1
      AA(LOC)=0.0
      IF(NK.GT.NEF+NET) GO TO  750
      SAVE=BB(NK,NJ)/WR(NJ)
      AA(LOC)=SAVE
  750 CONTINUE
  800 CONTINUE
C
      NFILE(1) ='FAST'
      NFILE(2) ='U   '
      CALL WRITE(IDENT,AA,LOC)
 1000 CONTINUE
C
      NFILE(1) = 'FAST'
      NFILE(2) = 'P   '
      NFILE(3) = '    '
C
      RETURN
      END
