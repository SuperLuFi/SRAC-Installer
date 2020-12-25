      SUBROUTINE    MAFCON(LTH,LA,LD,FTEMP,FSIG0)
C
C     THIS ROUTINE READ 'CMMN0000' MEMBER FROM USER'S FAST LIBRARY
C     AND  SET INDEX AND ADDRESSING FOR CROSS SECTION DATA & F-TABLE
C
      DOUBLE PRECISION  JNEFST,FNEFST,JNMACR,FNMACR
      CHARACTER*4       NAMEP
C
      COMMON  /MAFCNL/ IOPT(20),JNEFST,FNEFST,JNMACR,FNMACR,NEF,IGT,
     +                 NMAT,KNMAX,MXMTX,MXTEMP,MXSIG0,LNMAX,IDS,NEF1,
     +                 MXREAC,NOUT1,NOUT2,NORES,NMP,NISOHM,ISNCAL,IPLMAX
     +                ,IGMAX,NET,NET5
      COMMON /MAFCNT / AMASS,SIGP,ICAPT,IFISS,IIRES,LTOT,IFS,IFTR,IFC,
     1                 IFF,IFE,IFER,NGMIN,NGMAX,NSIG,NTEMP,SIGC0
      COMMON /MAFWRK / IA(17000),NAMEP(2),LOCAM(11),LOCAF(6)
C
      DIMENSION        A(17000)
      EQUIVALENCE     (IA(1),A(1))
C
      DIMENSION        LTH(MXMTX),LA(MXMTX),LD(MXMTX)
      DIMENSION        FTEMP(MXTEMP),FSIG0(MXSIG0)
C
C     START OF PROCESS
C
      NAMEP(1) (1:1)  =  'C'
      CALL  ICLEA( LOCAM   , 11 ,  0 )
      CALL  ICLEA( LOCAF   ,  6 ,  0 )
      CALL  ICLEA( IA      , 41 ,  0 )
      CALL   READ( NAMEP(1), IA , 41 )
C
      ICAPT  = IA(1)
      IFISS  = IA(2)
      IIRES  = IA(3)
      LTOT   = IA(4)
      DO 100 I = 1,4
      LTH(I) = IA(4+I)
      LA(I)  = IA(8+I)
      LD(I)  = IA(12+I)
  100 CONTINUE
      IFS    = IA(17)
      IFTR   = IA(18)
      IFC    = IA(19)
      IFF    = IA(20)
      IFE    = IA(21)
      IFER   = IA(22)
      NGMIN  = IA(23)
      NGMAX  = IA(24)
      NSIG   = IA(25)
      NTEMP  = IA(26)
      AMASS  =  A(27)
      SIGP   =  A(28)
      SIGC0  =  A(29)
      DO 110 I = 1,4
  110 FTEMP(I) = A(29+I)
      DO 120 I = 1,8
  120 FSIG0(I) = A(33+I)
C
CM    WRITE(NOUT2,301) IDENT
CM    WRITE(NOUT2,302) AMASS,SIGP,SIGC0,ICAPT,IFISS,IIRES,LTOT,
CM   +                (LTH(I),I = 1,MXMTX),(LA(I),I = 1,MXMTX),
CM   +                (LD(I),I = 1,MXMTX),
CM   +                 IFS,IFTR,IFC,IFF,IFE,IFER,NGMAX,NGMIN,NSIG,NTEMP,
CM   +                (FTEMP(I),I = 1,MXTEMP),(FSIG0(I),I = 1,MXSIG0)
C
C
C  >> SET ADDRESSING OF EACH REACTION X-SECTION DATA
C
      IF(LTOT.LE.0)   RETURN
C
      LOCAM(5) = 1
      IF(ICAPT.EQ.1)  THEN
                      LOCAM(1) = 1
                      LOCAM(5) = LOCAM(1)+NEF
                      ENDIF
      IF(IFISS.EQ.1)  THEN
                      LOCAM(2) = 1 + NEF*ICAPT
                      LOCAM(3) = LOCAM(2)+NEF
                      LOCAM(4) = LOCAM(3)+NEF
                      LOCAM(5) = LOCAM(4)+NEF
                      ENDIF
      LOCAM(6) = LOCAM(5)+NEF
      LOCAM(7) = LOCAM(6)+NEF
      IST      = LOCAM(7)+NEF
      DO 210 M = 1,4
      IF(LTH(M).LE.0) GO TO 210
      LOCAM(7+M) = IST
      IST        = IST+LTH(M)
  210 CONTINUE
C
      IF(IFS.EQ.0)  GO TO 230
      IST      = 1
      DO 220 M = 1,6
      GO TO (211,212,213,214,215,216),M
  211 IF(IFTR.EQ.0)  GO TO 220
      LOCAF(1) = IST
      GO TO 217
  212 IF(IFC.EQ.0)   GO TO 220
      LOCAF(2) = IST
      GO TO 217
  213 IF(IFF.EQ.0)   GO TO 220
      LOCAF(3) = IST
      GO TO 217
  214 IF(IFE.EQ.0)   GO TO 220
      LOCAF(4) = IST
      GO TO 217
  215 IF(IFER.EQ.0)  GO TO 220
      LOCAF(5) = IST
      GO TO 217
  216 CONTINUE
      LOCAF(6) = IST
  217 IST      = IST+NSIG*NTEMP*(NGMAX-NGMIN+1)
  220 CONTINUE
C
  230 CONTINUE
CM    WRITE(NOUT2,303) LOCAM
CM    WRITE(NOUT2,304) LOCAF
C
      RETURN
C
  301 FORMAT(1H ,20X,'## ',A8,'.CONT ## '//)
  302 FORMAT(1H ,15X,
     &         'AMASS ---------------------------- ',F9.3,
     &/1H ,15X,'SIGP ----------------------------- ',E12.5,
     &/1H ,15X,'SIGC0 ---------------------------- ',E12.5,
     &/1H ,15X,'ICAPT ---------------------------- ',I6,
     &/1H ,15X,'IFISS ---------------------------- ',I6,
     &/1H ,15X,'IIRES ---------------------------- ',I6,
     &/1H ,15X,'LTOT ----------------------------- ',I6,
     &/1H ,15X,'LTH ------------------------------ ',4I6,
     &/1H ,15X,'LA ------------------------------- ',4I6,
     &/1H ,15X,'LD ------------------------------- ',4I6,
     &/1H ,15X,'IFS ------------------------------ ',I6,
     &/1H ,15X,'IFTR IFC IFF IFE IFER ------------ ',5I6,
     &/1H ,15X,'NGMAX ---------------------------- ',I6,
     &/1H ,15X,'NGMIN ---------------------------- ',I6,
     &/1H ,15X,'NSIG ----------------------------- ',I6,
     &/1H ,15X,'NTEMP ---------------------------- ',I6,
     &/1H ,15X,'TEMP ----------------------------- ',4F9.2,
     &/1H ,15X,'SIG0 ----------------------------- ',1P8E9.1//)
  303 FORMAT(1H ,' ##LOCAM## ',11I8)
  304 FORMAT(1H ,' ##LOCAF## ', 6I8)
C
      END
