C             IRASIG              LEVEL=4        DATE=85.08.19
      SUBROUTINE  IRASIG(NISO  ,XL    ,DC    ,SIGTOT,IDENT ,
     &                   IRES  ,DN    ,LTH   ,LA    ,LD    ,
     &                   FTEMP ,FSIG0 ,PIJ   ,SSTF  ,SSTM  ,
     &                   SSSF  ,SSSM  ,AMF   ,AMM   ,SIGT  ,
     &                   POT   ,ISWM  ,MTNAME,ENBND ,DNM   ,
     &                   VOL )
C
      DOUBLE PRECISION  JNEFST,FNEFST,JNMACR,FNMACR
C
      COMMON /IRACNL/ IOPT(20),JNEFST,FNEFST,JNMACR,FNMACR,NEF,IGT,
     &                NMAT,KNMAX,MXMTX,MXTEMP,MXSIG0,LNMAX,IDS,NEF1,
     &                MXREAC,NOUT1,NOUT2
C
      COMMON /IRACNT/ AMASS,SIGP,ICAPT,IFISS,IIRES,LTOT,IFS,IFTR,IFC,
     &                IFF,IFE,IFER,NGMIN,NGMAX,NSIG,NTEMP,SIGC0
C
      COMMON /IRAWRK/ A(17000),NAMEP(2),LOCAM(11),LOCAF(6)
      DIMENSION      IA(17000),IDENTM(2,100)
      EQUIVALENCE    (A(1),IA(1))
C
      COMMON /IRADAT/ ISTART,NEFR,NEFB,NOMTF,NISOM,TCOR,S,NRES,
     &                NPSE,VOLF,VOLM,KKNMAX,NMP,MATD(50)
C
      COMMON /PDSPDS/ BUFFER(540),IFLSW,NFILE(3),ECODE,TTEMP
C
      COMMON /MAINC/IDUM(94),LINTOT,IDUM1(405)
C
      CHARACTER *4 IDPQ,NAMEP,IDENT,IDENTM,NFILE
      DIMENSION  IDPQ(2)
C
C     DIMENSION  NISO(NMAT),XL(NMAT),DC(NMAT),SIGTOT(NMAT),
      DIMENSION  NISO(NMAT),XL(NMAT),SIGTOT(NMAT),
     &           IDENT(2,KNMAX,NMAT),IRES(KNMAX,NMAT),DN(KNMAX,NMAT),
     &           LTH(MXMTX),LA(MXMTX),LD(MXMTX),FTEMP(MXTEMP),
     &           FSIG0(MXSIG0),PIJ(NMAT,NMAT),POT(KNMAX),
     &           SSTF(KNMAX,NEFR),SSTM(NISOM,NEFR),
     &           SSSF(KNMAX,NEFR),SSSM(NISOM,NEFR),
     &           AMF(KNMAX),AMM(NISOM),DNM(NISOM),ISWM(NISOM),
     &           MTNAME(2,NMAT),ENBND(NEF1),SIGT(NEF),VOL(NMAT)
CJAIS MODIFIED FOR NUCLIDE-WISE DANCOFF FACTOR ***4/8/1985***
      DIMENSION  DC(KNMAX,NMAT)
C
      DATA  IDPQ/'   C','   M'/
C
      ALFA(XX)=((XX-1.0)/(XX+1.0))**2
      GUZII(XX)=1.0+(XX/(1.0-XX))*ALOG(XX)
C
      LENG1=KNMAX*NEFR
      LENG2=NISOM*NEFR
C
      CALL  CLEA(SSTF,LENG1,0.0)
      CALL  CLEA(SSTM,LENG2,0.0)
      CALL  CLEA(SSSF,LENG1,0.0)
      CALL  CLEA(SSSM,LENG2,0.0)
      CALL  CLEA(AMF ,KNMAX,0.0)
      CALL  CLEA(AMM ,NISOM,0.0)
      CALL  CLEA(DNM ,NISOM,0.0)
      CALL ICLEA(ISWM,NISOM,0  )
C
C
      CALL  CLEA(SIGT  ,NEF  ,0.0)
      CALL  CLEA(SIGTOT,NMAT ,0.0)
      ICNT =0
      IPOS =0
C     IFLSW=1
C     NFILE(1)=4HFAST
C     NFILE(2)=4HU
C     NFILE(3)=4H
C
      DO 300 K=1,NMP
      NN=MATD(K)
      MMK=NISO(NN)
      IF(MMK.LE.0)     GO TO 300
      IF(NN.NE.NOMTF)  GO TO 251
C
      IPOS=K
      DO 250 M=1,MMK
C     IF(IRES(M,NN).EQ.-1) GO TO 221
      NAMEP(1)=IDENT(1,M,NN)
      NAMEP(2)=IDENT(2,M,NN)
      CALL PACK(NAMEP(1),1,IDPQ(1))
      IF(IRES(M,NN).NE.1) THEN
      NFILE(1)='FAST'
      NFILE(2)='U   '
      NAMEP(2)(1:1)='0'
      NAMEP(2)(4:4)='0'
                          ELSE
                    NFILE(1)='MICR'
                    NFILE(2)='EF  '
                    NAMEP(1)(1:1) = 'C'
                    NAMEP(2)(1:1) = 'F'
C@ADD
                    KANS     = 0
                    CALL  SEARCH(NAMEP(1),LENG,KANS)
                    IF(KANS.EQ.1)  THEN
                                   NAMEP(2)(4:4) = '0'
                                   IDENT(2,M,NN) (4:4) = '0'
                                   JANS = 0
                         CALL  SEARCH(NAMEP(1),LENG,JANS)
                         IF(JANS.EQ.1) THEN
                                       NFILE(1) = 'FAST'
                                       NFILE(2) = 'U   '
                                       NAMEP(2) = '0000'
                                       IDENT(2,M,NN) = '0000'
                                       IRES(M,NN) = 0
                                       ENDIF
                                   ENDIF
C@END
                      ENDIF
      CALL IRACON(LTH,LA,LD,FTEMP,FSIG0)
      AMF(M)=AMASS
      POT(M)=SIGP
C     IF(IRES(M,NN).EQ.-1) GO TO 221
      IF(LTOT.LE.0)  GO TO 221
      ALPHA=ALFA(AMASS)
      IF(ALPHA.LE.0.001)  SLDWN=1.0
      IF(ALPHA.GT.0.001)  SLDWN=GUZII(ALPHA)
      DNTMP=DN(M,NN)
      CALL PACK(NAMEP(1),1,IDPQ(2))
      CALL READ(NAMEP(1),A,LTOT)
      IST=LOCAM(5)-1
      DO 210 I=1,NEF
      SIGT(I)=SIGT(I)+A(IST+I)*DNTMP
      IF(I.GT.NEFB) SSTF(M,I-NEFB)=A(IST+I)
  210 CONTINUE
      SIGTOT(K)=SIGTOT(K)+DNTMP*A(IST+NEF)
      IST=LOCAM(7)-1
      DO 215 I=ISTART,NEF
      SSSF(M,I-NEFB)=SLDWN*DNTMP*A(IST+I)
  215 CONTINUE
  221 CONTINUE
C --- TRANPORT CORRECTION OF TOTAL CROSS SECTION FOR PIJ-ROUTINE
      IST=LOCAM(11)-1
      IDWN=LD(4)+1
      IEXT=LA(4)
      IF(IST.LE.0.OR.IDWN.LE.0.OR.IEXT.LE.0)  GO TO 245
      SAVEP1=0.0
      DO 240 I=1,IEXT
      IF(I.EQ.NEF)  GO TO 229
      IST=IST+IDWN
      GO TO 240
  229 DO 230 J=1,IDWN
      IST=IST+1
      SAVEP1=SAVEP1+A(IST)
  230 CONTINUE
  240 CONTINUE
C     SIGTOT(K)=SIGTOT(K)-SAVEP1*DNTMP*0.333333
  245 CONTINUE
  250 CONTINUE
      GO TO 300
C
  251 CONTINUE
      DO 290 M=1,MMK
      ICNT=ICNT+1
      NAMEP(1)=IDENT(1,M,NN)
      NAMEP(2)=IDENT(2,M,NN)
      IDENTM(1,ICNT)=NAMEP(1)
      IDENTM(2,ICNT)=NAMEP(2)
C     IF(IRES(M,NN).EQ.-1) GO TO 289
      CALL PACK(NAMEP(1),1,IDPQ(1))
      IF(IRES(M,NN).NE.1) THEN
      NFILE(1)='FAST'
      NFILE(2)='U   '
      NAMEP(2)(1:1)='0'
      NAMEP(2)(4:4)='0'
                    ELSE
                    NFILE(1)='MICR'
                    NFILE(2)='EF  '
                    NAMEP(1)(1:1) = 'C'
                    NAMEP(2)(1:1) = 'F'
C@ADD
                    KANS     = 0
                    CALL  SEARCH(NAMEP(1),LENG,KANS)
                    IF(KANS.EQ.1)  THEN
                                   NAMEP(2)(4:4) = '0'
                                   IDENT(2,M,NN) (4:4) = '0'
                                   JANS = 0
                         CALL  SEARCH(NAMEP(1),LENG,JANS)
                         IF(JANS.EQ.1) THEN
                                       NFILE(1) = 'FAST'
                                       NFILE(2) = 'U   '
                                       NAMEP(2) = '0000'
                                       IDENT(2,M,NN) = '0000'
                                       IRES(M,NN) = 0
                                       ENDIF
                                   ENDIF
C@END
                    ENDIF
C
      CALL IRACON(LTH,LA,LD,FTEMP,FSIG0)
      AMM(ICNT)=AMASS
      DNTMP=DN(M,NN)
      DNM(ICNT)=DNTMP*VOL(NN)/VOLM
      ISWM(ICNT)=IRES(M,NN)
C     IF(IRES(M,NN).EQ.-1) GO TO 289
      IF(LTOT.LE.0)        GO TO 289
      ALPHA=ALFA(AMASS)
      IF(ALPHA.LE.0.001)  SLDWN=1.0
      IF(ALPHA.GT.0.001)  SLDWN=GUZII(ALPHA)
      CALL PACK(NAMEP(1),1,IDPQ(2))
      CALL READ(NAMEP(1),A,LTOT)
      IST=LOCAM(5)-1
      DO 260 I=ISTART,NEF
      IF(I.EQ.NEF ) SIGTOT(K)=SIGTOT(K)+DNTMP*A(IST+I)
      IF(I.GT.NEFB) SSTM(ICNT,I-NEFB)=A(IST+I)
  260 CONTINUE
      IST=LOCAM(7)-1
      DO 270 I=ISTART,NEF
      SSSM(ICNT,I-NEFB)=SLDWN*DNM(ICNT)*A(IST+I)
  270 CONTINUE
C --- TRANPORT CORRECTION OF TOTAL CROSS SECTION FOR PIJ-ROUTINE
      IST=LOCAM(11)-1
      IDWN=LD(4)+1
      IEXT=LA(4)
      IF(IST.LE.0.OR.IDWN.LE.0.OR.IEXT.LE.0)  GO TO 289
      SAVEP1=0.0
      DO 280 I=1,IEXT
      IF(I.EQ.NEF)  GO TO 277
      IST=IST+IDWN
      GO TO 280
  277 DO 279 J=1,IDWN
      IST=IST+1
      SAVEP1=SAVEP1+A(IST)
  279 CONTINUE
  280 CONTINUE
C     SIGTOT(K)=SIGTOT(K)-SAVEP1*DNTMP*0.333333
  289 CONTINUE
  290 CONTINUE
C
  300 CONTINUE
      IF(IOPT(19).EQ.0) GO TO 400
C
C ---- COMPOSITION DATA LIST
C
      WRITE(NOUT2,1001)
      WRITE(NOUT2,1002) MTNAME(1,NOMTF),MTNAME(2,NOMTF)
      NISOF=NISO(NOMTF)
      WRITE(NOUT2,1003) NOMTF,IPOS,NISOF,VOLF
      WRITE(NOUT2,1010) ((IDENT(J,I,NOMTF),J=1,2),I=1,NISOF)
      WRITE(NOUT2,1004) (AMF(I),I=1,NISOF)
      WRITE(NOUT2,1005) (DN(I,NOMTF),I=1,NISOF)
      WRITE(NOUT2,1006) (POT(I),I=1,NISOF)
      WRITE(NOUT2,1009) (IRES(I,NOMTF),I=1,NISOF)
      WRITE(NOUT2,1011) (DC  (I,NOMTF),I=1,NISOF)
C
      WRITE(NOUT2,1007)
      WRITE(NOUT2,1008) NISOM,VOLM
      WRITE(NOUT2,1010) ((IDENTM(J,I),J=1,2),I=1,NISOM)
      WRITE(NOUT2,1004) (AMM(I),I=1,NISOM)
      WRITE(NOUT2,1005) (DNM(I),I=1,NISOM)
      WRITE(NOUT2,1009) (ISWM(I),I=1,NISOM)
C
 1001 FORMAT(1H1//1H ,30X,' COMPOSITION DATA LIST AFTER ARRANGEMENT  ',
     &' (DATA FROM IRA-ROUTINE)'//)
 1002 FORMAT(1H ,20X,' ## FUEL REGION ## ',
     &     //1H ,10X,' FUEL MATERIAL NAME  ----------------- : ',2A4)
 1003 FORMAT(1H ,10X,' MATERIAL NUMBER --------------------- : ',I3,
     &      /1H ,10X,' M-REGION NUMBER --------------------- : ',I3,
     &      /1H ,10X,' NUMBER OF NUCLIDE ------------------- : ',I3,
     & /1H ,10X,' FUEL VOLUME ------------------------- : ',1PE12.5)
 1004 FORMAT(1H ,10X,' ATOMIC MASS FOR EACH NUCLIDE -------- : ',
     &1P6E12.5,/,(1H ,51X,1P6E12.5))
 1005 FORMAT(1H ,10X,' NUMBER DENSITY FOR EACH NUCLIDE ----- : ',
     &1P6E12.5,/,(1H ,51X,1P6E12.5))
 1006 FORMAT(1H ,10X,' POTENTIAL SCATTERING FOR EACH NUCLIDE : ',
     &1P6E12.5,/,(1H ,51X,1P6E12.5))
 1009 FORMAT(1H ,10X,' RESONANT INDICATOR FOR EACH NUCLIDE - : ',
     &6(I4,8X),/,(1H ,51X,6(I4,8X)))
 1010 FORMAT(1H ,10X,' NUCLIDE NAME FOR EACH NUCLIDE ------- : ',
     &6(2A4,4X),/,(1H ,52X,6(2A4,4X)))
 1011 FORMAT(1H ,10X,' DANCOFF FACTOR FOR EACH NUCLIDE ----- : ',
     &6(F9.5,3X),/,(1H ,51X,6(F9.5,3X) )  )
 1007 FORMAT(//1H ,20X,'## MODERATOR REGION ## '/)
 1008 FORMAT(1H ,10X,' NUMBER OF NUCLIDE ------------------- : ',I3,
     & /1H ,10X,' MODERATOR VOLUME -------------------- : ',1PE12.5)
C
  400 DO 500 I=1,NISOM
      IF(ISWM(I).NE.-1)  ISWM(I)=1
      IF(ISWM(I).EQ.-1)  ISWM(I)=0
  500 CONTINUE
CJAIS DELETE STATEMENTS OF DANCOFF FACTOR CALCULATION  ***4/8/1985
C     CALCULATED DANCOFF FACTOR IS PASSED BY LABELED COMMON /NEWDAN/.
C
C
C     IF(IOPT(3).EQ.0)  RETURN
C
C     DANCOFF CORRECTION FACTOR CALCULATION
C
C     IF(IOPT(3).EQ.1) GO TO 1200
C
C     DANCOFF CORRECTION FACTOR BY EMPIRICAL FORMULA
C
C
C     WRITE(NOUT1,2003)
C     STOP
C2003 FORMAT(//1H ,10X,'ERROR STOP ---- AT SUB. IRASIG'
C    +        /1H ,10X,'           ---- EMPIRICAL FORMULA OF DANCOFF FAC
C    +TOR IS NOT GIVEN IN SRAC SYSTEM')
C
C     DANCOFF CORRECTION FACTOR BY THE COLLISION PROBABILITY
C
C1200 CONTINUE
C     XLL      = XL(NOMTF)
C     IF(XLL.EQ.0.0)  RETURN
C     DC(NOMTF)=   0.0
C     SIGTIN   = 100./XLL
C
C     SIGTOT(IPOS)=SIGTIN
C
C     NG=1
C
C     REWIND LINTOT
C     DO 1250 N=1,NMP
C     WRITE(LINTOT)   SIGTOT(N)
C1250 CONTINUE
C     REWIND LINTOT
C
C     CALL PIJ2(NG,3)
C     CALL CLEA(PIJ,NMAT*NMAT,0.0)
C
C     REWIND 21
C     READ(21) ((PIJ(I,J),I=1,NMP),J=1,NMP)
C     REWIND 21
C
C     DC(NOMTF)=1.0-(1.0-PIJ(IPOS,IPOS))*XLL*SIGTIN
C
C     WRITE(NOUT2,2021)
C     WRITE(NOUT2,2022)  MTNAME(1,NOMTF),MTNAME(2,NOMTF),
C    *                   DC(NOMTF),XLL,PIJ(IPOS,IPOS),NEF
C     IF(IOPT(19).GT.0)
C    *WRITE(NOUT2,2023) (MATD(K),SIGTOT(K),K=1,NMP)
C
C2021 FORMAT(//1H ,10X,' ## DANCOFF FACTOR CALCULATION (BY COLLISION PRO
C    +BABILITY ) ## '/)
C2022 FORMAT(1H ,15X,'REGION NAME -------------------- ',1X,2A4,
C    +      /1H ,15X,'DANCOFF FACTOR ----------------- ',E12.5,
C    +      /1H ,15X,'MEAN CHORD LENGTH -------------- ',E12.5,
C    +      /1H ,15X,'PIJ(K,K) ----------------------- ',E12.5,
C    +      /1H ,15X,'ENERGY GROUP NO ---------------- ',I4)
C2023 FORMAT(1H ,15X,'TOTAL X-SECTION FOR PIJ2-ROUTINE :',/,
C    +(1H ,20X,5(I3,2X,1PE12.5)))
C
      RETURN
      END
