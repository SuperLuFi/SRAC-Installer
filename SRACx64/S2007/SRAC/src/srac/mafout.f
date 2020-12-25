C***********************************************************************
C                           MAFOUT
C***********************************************************************
      SUBROUTINE MAFOUT(IDENT ,LSS   ,LGV   ,SIGC  ,SIGF  ,SIGFNU,SIGT
     &                 ,CHI   ,D1    ,D2    ,SIGA  ,SIGS  ,ENBND ,IPL
     &                 ,ID8TH ,IOPT19)
C
C     PRINT-OUT AND DISK-OUT OF EFFECTIVE MACRO. CROSS SECTION
C
      DOUBLE PRECISION  JNEFST,FNEFST,JNMACR,FNMACR
      CHARACTER*4       NAMEP,NFILE
C
      COMMON  /MAFCNL/ IOPT(20),JNEFST,FNEFST,JNMACR,FNMACR,NEF,IGT,
     +                 NMAT,KNMAX,MXMTX,MXTEMP,MXSIG0,LNMAX,IDS,NEF1,
     +                 MXREAC,NOUT1,NOUT2,NORES,NMP,NISOHM,ISNCAL,IPLMAX
     +                ,IGMAX,NET,NET5
      COMMON  /MAFWRK/ IA(17000),NAMEP(2),LOCAM(11),LOCAF(6)
      COMMON  /PDSPDS/ BUFFER(540),IFLSW,NFILE(3),ECODE,ITEMP
C
      DIMENSION        LSS(NEF),LGV(NEF),SIGC(NEF),SIGF(NEF)
      DIMENSION        SIGT(NEF),CHI(NEF),D1(NEF),D2(NEF),SIGA(NEF)
      DIMENSION        SIGS(IDS,NEF),ENBND(NEF1),SIGFNU(NEF)
      DIMENSION        STORE(17000)
      CHARACTER*4      IDENT(2),ID8TH
C
C
      EQUIVALENCE     (IA(1),STORE(1))
C
C     INITIAL SET
C
      CALL ICLEA( IA   , 17000 , 0 )
      CALL ICLEA( LSS  , NEF   , 1 )
C     MODIFIED BY TSUCHI TO ACCEPT NULL N2N ARRAY
      CALL ICLEA( LGV  , NEF   , 1 )
      NAMEP(1) = IDENT(1)
      NAMEP(2) = IDENT(2)
      NAMEP(2) (1:1) = 'F'
      NAMEP(2) (4:4) = ID8TH (4:4)
C
      SUM      = 0.0
      DO 100 I = 1,NEF
      SUM      = SUM + CHI(I)
      DO  10 J = 1,IDS
      JJ       = IDS + 1 - J
      IF(SIGS(JJ,I).EQ.0.0) GO TO 10
      LGV(I)   = JJ
      GO TO 100
   10 CONTINUE
  100 CONTINUE
C
      LTH      = 0
      FACT     = 1.0
      IF(SUM.NE.0.0) FACT = 1.0/SUM
C
      DO 200 I = 1,NEF
      CHI(I)   = CHI(I)*FACT
      LTH      = LTH + LGV(I)
  200 CONTINUE
C
      LTH      = LTH + 10*NEF
C
C     PRINT-OUT
C
      IPRT     = NOUT2
      I1       = 0
      JSW      = 0
      DO 300 I = 1,NEF
      IF(IOPT19.GT.0) THEN
               IF(MOD(JSW,40).EQ.0)  THEN
                      IF(ID8TH(4:4).EQ.'M') THEN
                                            WRITE(IPRT,1007) IDENT
                                            ELSE
                                            WRITE(IPRT,1000) IDENT,IPL
                                            ENDIF
                                     WRITE(IPRT,1001)
                                     WRITE(IPRT,1005)
                                     ENDIF
                       IF(SIGT(I).NE.0.0) THEN
                                          JSW     = JSW + 1
                                          WRITE(IPRT,1002)
     +                                    I,SIGC(I),SIGF(I),
     +                                    SIGFNU(I),SIGT(I),CHI(I),
     +                                    D1(I),D2(I),SIGA(I)
                       IF(MOD(JSW,40).EQ.0) WRITE(IPRT,1005)
                                          ENDIF
                       ENDIF
C
      IA(I1+1)    = LSS(I)
      IA(I1+2)    = LGV(I)
      STORE(I1+3) = SQRT(2./(ENBND(I)+ENBND(I+1)))
      STORE(I1+4) = SIGF(I)
      STORE(I1+5) = SIGFNU(I)
      STORE(I1+6) = SIGT(I)
      STORE(I1+7) = CHI(I)
      STORE(I1+8) = D1(I)
      STORE(I1+9) = D2(I)
      STORE(I1+10)= SIGA(I)
C
      I1          = I1 + 10
      ISW         = LGV(I)
      IF(ISW.LE.0)  GO TO 300
      DO 250    J = 1,ISW
      I1          = I1 + 1
      STORE(I1)   = SIGS(J,I)
  250 CONTINUE
  300 CONTINUE
      IF(MOD(JSW,40).NE.0) WRITE(IPRT,1005)
C
      ISW=1
      IF(IOPT19.LE.1)  GO TO 330
  310 CONTINUE
      LCNT = 0
      WRITE(IPRT,1000) IDENT,IPL
      WRITE(IPRT,1006)
      LCNT = 5
  320 CONTINUE
      NRCD = LGV(ISW)
      IF(NRCD.EQ.1.AND.SIGS(1,ISW).EQ.0.0) GO TO 325
      LENG = FLOAT(NRCD)/10.0 +0.999
      IF(NRCD.LE.0)  LENG=0
      LCNT = LCNT+LENG+2
      IF(LCNT.GT.56) GO TO 310
      WRITE(IPRT,1003) ISW,LSS(ISW),LGV(ISW)
      IF(NRCD.GT.0) WRITE(IPRT,1004) (SIGS(J,ISW),J=1,NRCD)
C
  325 ISW  = ISW+1
      IF(ISW.LE.NEF) GO TO 320
C
C     DISK-OUT
C
  330 CONTINUE
      IFLSW    = 1
      LENG     = LTH
      NFILE(1) = 'MACR'
      NFILE(2) = 'OWRK'
      CALL WRITE(NAMEP(1),IA,LENG)
      RETURN
C
 1000 FORMAT(1H1,/1H ,30X,'MATERIAL NAME = ',2A4,'  (#',I1,
     &'--0/N:ISOTROPIC/ANISOTROPIC)'/)
 1001 FORMAT(1H ,30X,'EFFECTIVE MACROSCOPIC CROSS SECTION'//1H ,10X,
     &'GRP. ',T17,'CAPTURE',T29,'FISSION',T41,'NU*FISSION',T53,
     &'TOTAL',T65,'CHI',T76,'D1',T89,'D2',T101,'ABSORPTION')
 1002 FORMAT(1H ,10X,I3,1P8E12.5)
 1003 FORMAT(/1H ,5X,'SCATTERING TRANSFER ------ SOURCE GROUP NO. = ',
     &I3,' (LSS =',I2,':LGV = ',I3,')')
 1004 FORMAT(1H ,4X,1P10E12.5)
 1005 FORMAT(1H ,7X,21(5H-----))
 1006 FORMAT(1H ,30X,'EFFECTIVE MACROSCOPIC CROSS SECTION'//)
C
 1007 FORMAT(1H1,/1H ,30X,'MATERIAL NAME = ',2A4,'  ( ',
     &'N2N REACTION)'/)
C
      END
