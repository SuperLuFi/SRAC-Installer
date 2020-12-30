      SUBROUTINE  MAFPRT(MTNAME,LTH   ,LA    ,LD    ,SSC   ,SSF   ,SSNU
     1                  ,SCHI  ,SSTR  ,SSE   ,SST   ,SSIN  ,SS2N  ,UM
     2                  ,SMTX  ,STR   ,SIG0  ,SFCTR ,ID    ,NN    ,MM
     3                  ,IOPT19,IMASK ,FMTX  ,RTEMP )
C
C      PRINT OUT  EFFECTIVE MICROSCOPIC CROSS SECTION
C
      DOUBLE PRECISION  JNEFST,FNEFST,JNMACR,FNMACR
      CHARACTER*4       NFILE,MTNAME,ID,ISAVE,NAMEP
C
      COMMON  /MAFCNL/ IOPT(20),JNEFST,FNEFST,JNMACR,FNMACR,NEF,IGT,
     +                 NMAT,KNMAX,MXMTX,MXTEMP,MXSIG0,LNMAX,IDS,NEF1,
     +                 MXREAC,NOUT1,NOUT2,NORES,NMP,NISOHM,ISNCAL,IPLMAX
     +                ,IGMAX,NET,NET5
      COMMON  /MAFCNT/ AMASS,SIGP,ICAPT,IFISS,IIRES,LTOT,IFS,IFTR,IFC,
     +                 IFF,IFE,IFER,NGMIN,NGMAX,NSIG,NTEMP,SIGC0
      COMMON  /MAFWRK/ A(17000),NAMEP(2),LOCAM(11),LOCAF(6)
C
      COMMON  /PDSPDS/ BUFFER(540),IFLSW,NFILE(3),ECODE,ITEMP
C
      DIMENSION        IA(17000)
      EQUIVALENCE      (IA(1),A(1))
C
      DIMENSION  MTNAME(2,NMAT),LTH(MXMTX),LA(MXMTX),LD(MXMTX),
     1           SSC(NEF),SSF(NEF),SSNU(NEF),SCHI(NEF),SSTR(NEF),
     2           SSE(NEF),SST(NEF),SSIN(NEF),SS2N(NEF),UM(NEF),
     3           SMTX(IDS,NEF),STR(IDS,NEF),SIG0(NEF,KNMAX),
     4           SFCTR(NEF,MXREAC),ID(2),FMTX(3,NEF)
C
C@MOD IF(IFS.EQ.0)  RETURN
C
      NAMEP(1) = ID(1)
      NAMEP(2) = ID(2)
      NAMEP(2) (2:3) = MTNAME(2,NN) (2:3)
C
      IDWN1   = 0
      IDWN2   = 0
      IDWN3   = 0
      IEXT1   = 0
      IEXT2   = 0
      IEXT3   = 0
      DO 10 J = 1,MXMTX
      ISW     = LD(J)+1
      IF(ISW.GT.IDWN1) IDWN1=ISW
      IF(J.EQ.3)       IDWN2=ISW
      IF(J.EQ.4)       IDWN3=ISW
      ISW     = LA(J)
      IF(ISW.GT.IEXT1) IEXT1=ISW
      IF(J.EQ.3)       IEXT2=ISW
      IF(J.EQ.4)       IEXT3=ISW
   10 CONTINUE
C
      IF(IMASK.EQ.0) THEN
CKSK                 NAMEP(2) = 4H0000
                     NAMEP(2) = '0000'
                     GO TO 301
                     ENDIF
C
      IF(IOPT19.LE.0)  GO TO 301
C
      DO 100 I= 1,NEF
      ISW     = I-1
      IF(MOD(ISW,25).EQ.0) WRITE(NOUT2,1001)  (MTNAME(J,NN),J=1,2),ID
      IF(MOD(ISW,25).EQ.0) WRITE(NOUT2,1002)
      IF(MOD(ISW,25).EQ.0) WRITE(NOUT2,1005)
      SSER    = 0.0
      DO 20 J = 2,IDWN2
      SSER    = SSER + SMTX(J,I)
   20 CONTINUE
      WRITE(NOUT2,1003) I,SST(I),SSTR(I),SSC(I),SSF(I),SSE(I),SSER,
     1                 SSIN(I),SS2N(I),SSNU(I),SCHI(I),UM(I)
      WRITE(NOUT2,1004) SIG0(I,MM),(SFCTR(I,J),J=1,5)
      IF(MOD(ISW,25).EQ.24.OR.I.EQ.NEF) WRITE(NOUT2,1005)
  100 CONTINUE
C
C     SCATTERING MATRICES PRINT-OUT
C
      IF(IOPT19.LE.1)  GO TO 301
C
      ISKIP    = 0
      IF(ISKIP.EQ.0)  GO TO 201
      IF(IDWN1.LE.0.OR.IEXT1.LE.0)  GO TO 201
      JL       = FLOAT(IDWN1)/10.+0.999
      KL       = 48./FLOAT(JL)
      KPAGE    = FLOAT(IEXT1)/FLOAT(KL) + 0.999
      DO 200 I = 1,KPAGE
      CALL  MAFTTL(ID,1,I,KPAGE,NOUT2)
      CALL  MAFLTM(STR,IEXT1,IDWN1,KL,I,IDS,NEF,NOUT2)
  200 CONTINUE
C
C     ELASTIC STATTERING MATRICES PRINT-OUT
C
  201 CONTINUE
      IF(IDWN2.LE.0.OR.IEXT2.LE.0)  GO TO 301
      JL        = FLOAT(IDWN2)/10.+0.999
      KL        = 48./FLOAT(JL)
      KPAGE     = FLOAT(IEXT2)/FLOAT(KL) + 0.999
      DO 300  I = 1,KPAGE
      CALL  MAFTTL(ID,2,I,KPAGE,NOUT2)
      CALL  MAFLTM(SMTX,IEXT2,IDWN2,KL,I,IDS,NEF,NOUT2)
  300 CONTINUE
C
C     END
C
  301 CONTINUE
*     WRITE(6,*) ' ** IMASK NAMEP (MAFPRT) : ',IMASK,ID
      CALL ICLEA(IA    ,42    ,0)
      IA(1) = ICAPT
      IA(2) = IFISS
      IA(3) = 0
      IA(4) = LTOT
      IA(25)= 0
      IA(26)= 0
      A(27) = AMASS
      A(28) = SIGP
      A(29) = SIGC0
      A(30) = RTEMP
      DO  310 I = 1,4
      IA(4+I)   = LTH(I)
      IA(8+I)   = LA(I)
      IA(12+I)  = LD(I)
  310 CONTINUE
      IF(LTH(4).GT.0)  IA(42) = 1
C
      NFILE(1) = 'MICR'
      NFILE(2) = 'EF  '
      NAMEP(1) (1:1) = 'C'
      NAMEP(2) (1:1) = 'F'
      LENG     = 0
      ISW      = 0
      CALL SEARCH(NAMEP(1),LENG,ISW)
      IF(ISW.EQ.0) THEN
                   WRITE(NOUT2,302) NAMEP,MTNAME(1,NN),MTNAME(2,NN)
                   RETURN
                   ENDIF
C
      CALL WRITE(NAMEP(1),IA,42)
C
      ISAVE    = NAMEP(2)
      NAMEP(1) (1:1) = 'M'
      NAMEP(2) = '0000'
      NFILE(1) = 'FAST'
      NFILE(2) = 'U   '
C
      CALL  READ(NAMEP(1),A,LTOT)
C --- STORE EFFECTIVE MICROSCOPIC X-SECTION
      ISTC     = LOCAM(1)-1
      ISTF     = LOCAM(2)-1
      ISTT     = LOCAM(5)-1
      ISTW     = LOCAM(6)-1
      ISTE     = LOCAM(7)-1
      DO 320 I = 1,NEF
      IF(ICAPT.EQ.1)  A(ISTC+I) = SSC(I)
      IF(IFISS.EQ.1)  A(ISTF+I) = SSF(I)
C *** STORES TRANSPORT X-SECTION
      A(ISTT+I)= SSTR(I)
C *** STORES TOTAL X-SECTION INSTEAD OF WEIGHTING FLUX
      A(ISTW+I)= SST(I)
      A(ISTE+I)= SSE(I)
  320 CONTINUE
      IST      = LOCAM(10)-1
      IF(IST.LE.0)  GO TO 340
      DO 330 I = 1,IEXT2
      DO 330 J = 1,IDWN2
      IST      = IST + 1
      A(IST)   = SMTX(J,I)
  330 CONTINUE
C
  340 CONTINUE
      IST      = LOCAM(11)-1
      IF(IST.LE.0)  GO TO 360
C
      DO 350 I = 1,IEXT3
      FACT     = FMTX(1,I)
      IST      = IST + 1
      A(IST)   = A(IST)*FACT
      IF(IDWN3.GT.1) THEN
                     FACT     = FMTX(2,I)
                     DO 345 J = 2,IDWN3
                     IST      = IST + 1
                     A(IST)   = A(IST)*FACT
  345                CONTINUE
                     ENDIF
  350 CONTINUE
C
C
  360 CONTINUE
      NAMEP(2) = ISAVE
      NFILE(1) = 'MICR'
      NFILE(2) = 'EF  '
      CALL WRITE(NAMEP(1),A,LTOT)
C
C     EFFECTIVE MICROSCOPIC X-SECTION DISK-OUT END
C                              INTO PDS-FILE (MICREF)
C
      NFILE(1)='FAST'
      NFILE(2)='U   '
      RETURN
C
  302 FORMAT(1H ,'  WARNING ----> ',2A4,'(',2A4,') ALREADY EXISTS IN ',
     &'MICREF-FILE. OUTPUT OF EFFECTIVE CROSS SECTION IS SKIPPED ]] ')
C
 1001 FORMAT(1H1/1H ,30X,'MATERIAL NAME = ',2A4,5X,'NUCLIDE NAME = ',
     &2A4//)
 1002 FORMAT(1H ,'GRP. ',T7,'TOTAL',T18,'TRANSPORT',T29,'CAPTURE',
     &T40,'FISSION',T51,'ELASTIC',T62,'EL REMOVAL',T73,'INELA.',
     &T84,'N2N',T95,'NU',T106,'CHI',T117,'MU')
 1003 FORMAT(1H ,I3,1P11E11.4)
 1004 FORMAT(1H ,3X,E11.4,5F11.5)
 1005 FORMAT(1H ,1X,25(5H-----))
 1006 FORMAT(1H1//)
C
      END
