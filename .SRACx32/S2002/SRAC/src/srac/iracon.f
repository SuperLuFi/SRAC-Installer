      SUBROUTINE    IRACON(LTH,LA,LD,FTEMP,FSIG0)
C
      DOUBLE PRECISION  JNEFST,FNEFST,JNMACR,FNMACR
C
      COMMON /IRACNL/ IOPT(20),JNEFST,FNEFST,JNMACR,FNMACR,NEF,IGT,
     &                NMAT,KNMAX,MXMTX,MXTEMP,MXSIG0,LNMAX,IDS,NEF1,
     &                MXREAC,NOUT1,NOUT2
C
      COMMON /IRACNT/ AMASS,SIGP,ICAPT,IFISS,IRES,LTOT,IFS,IFTR,IFC,
     &                IFF,IFE,IFER,NGMIN,NGMAX,NSIG,NTEMP,SIGC0
C
      COMMON /IRAWRK/ IA(17000),NAMEP(2),LOCAM(11),LOCAF(6)
      DIMENSION        A(17000)
      EQUIVALENCE     (IA(1),A(1))
C
      DIMENSION   LTH(MXMTX),LA(MXMTX),LD(MXMTX)
      DIMENSION   FTEMP(MXTEMP),FSIG0(MXSIG0)
      DIMENSION  IDPQ(2)
CADD SASAQ
      CHARACTER*4 IDPQ
      DATA  IDPQ/ '   C','   0'/
C
C
CJ    SYN FST1=JNEFST.FNEFST.IDENT.'CONT'
C
      CALL  PACK(NAMEP(1),1,IDPQ(1))
C     CALL  PACK(NAMEP(2),1,IDPQ(2))
C     CALL  PACK(NAMEP(2),4,IDPQ(2))
      CALL   READ(NAMEP(1),IA,41)
C
      ICAPT=IA(1)
      IFISS=IA(2)
      IRES =IA(3)
      LTOT =IA(4)
      DO 202 I=1,4
      LTH(I)=IA(4+I)
      LA(I) =IA(8+I)
      LD(I) =IA(12+I)
  202 CONTINUE
      IFS  =IA(17)
      IFTR =IA(18)
      IFC  =IA(19)
      IFF  =IA(20)
      IFE  =IA(21)
      IFER =IA(22)
      NGMIN=IA(23)
      NGMAX=IA(24)
      NSIG =IA(25)
      NTEMP=IA(26)
      AMASS= A(27)
      SIGP = A(28)
      SIGC0= A(29)
      DO 205 I=1,4
  205 FTEMP(I)=A(29+I)
      DO 206 I=1,8
  206 FSIG0(I)=A(33+I)
C
C
CJ    READ(FST1,ERR=301) AMASS,SIGP,SIGC0,ICAPT,IFISS,IRES,LTOT,
CJ   +             (LTH(I),I=1,MXMTX),(LA(I),I=1,MXMTX),
CJ   +             (LD(I),I=1,MXMTX),
CJ   +             IFS,IFTR,IFC,IFF,IFE,IFER,NGMAX,NGMIN,NSIG,
CJ   +             NTEMP,(FTEMP(I),I=1,MXTEMP),(FSIG0(I),I=1,MXSIG0)
C
C     WRITE(NOUT2,203) IDENT
C 203 FORMAT(1H ,20X,'## ',A8,'.CONT ## '//)
C     WRITE(NOUT2,204) AMASS,SIGP,SIGC0,ICAPT,IFISS,IRES,LTOT,
C    +             (LTH(I),I=1,MXMTX),(LA(I),I=1,MXMTX),
C    +             (LD(I),I=1,MXMTX),
C    +             IFS,IFTR,IFC,IFF,IFE,IFER,NGMAX,NGMIN,NSIG,
C    +             NTEMP,(FTEMP(I),I=1,MXTEMP),(FSIG0(I),I=1,MXSIG0)
C 204 FORMAT(1H ,15X,
C    1         'AMASS ---------------------------- ',F9.3,
C    2/1H ,15X,'SIGP ----------------------------- ',E12.5,
C    3/1H ,15X,'SIGC0 ---------------------------- ',E12.5,
C    4/1H ,15X,'ICAPT ---------------------------- ',I6,
C    5/1H ,15X,'IFISS ---------------------------- ',I6,
C    +/1H ,15X,'IRES ----------------------------- ',I6,
C    6/1H ,15X,'LTOT ----------------------------- ',I6,
C    7/1H ,15X,'LTH ------------------------------ ',4I6,
C    8/1H ,15X,'LA ------------------------------- ',4I6,
C    9/1H ,15X,'LD ------------------------------- ',4I6,
C    A/1H ,15X,'IFS ------------------------------ ',I6,
C    B/1H ,15X,'IFTR IFC IFF IFE IFER ------------ ',5I6,
C    C/1H ,15X,'NGMAX ---------------------------- ',I6,
C    D/1H ,15X,'NGMIN ---------------------------- ',I6,
C    E/1H ,15X,'NSIG ----------------------------- ',I6,
C    F/1H ,15X,'NTEMP ---------------------------- ',I6,
C    G/1H ,15X,'TEMP ----------------------------- ',4F9.2,
C    H/1H ,15X,'SIG0 ----------------------------- ',1P8E9.1//)
C
      CALL  ICLEA(LOCAM,11,0)
      CALL  ICLEA(LOCAF, 6,0)
      IF(LTOT.LE.0)  RETURN
      IF(ICAPT.EQ.1)  LOCAM(1)=1
      IF(IFISS.EQ.0)  GO TO 210
      LOCAM(2)=1+NEF*ICAPT
      LOCAM(3)=LOCAM(2)+NEF
      LOCAM(4)=LOCAM(3)+NEF
  210 CONTINUE
      LOCAM(5)=LOCAM(4)+NEF
      IF(IFISS.EQ.0)  LOCAM(5)=LOCAM(1)+NEF
      IF(ICAPT.EQ.0.AND.IFISS.EQ.0)  LOCAM(5)=1
      LOCAM(6)=LOCAM(5)+NEF
      LOCAM(7)=LOCAM(6)+NEF
      IST=LOCAM(7)+NEF
      DO 211 M=1,4
      IF(LTH(M).LE.0) GO TO 211
      LOCAM(7+M)=IST
      IST=IST+LTH(M)
  211 CONTINUE
C
      IF(IFS.EQ.0)  GO TO 220
      IST=1
      DO 219 M=1,6
      GO TO (221,222,223,224,225,226),M
  221 IF(IFTR.EQ.0)  GO TO 219
      LOCAF(1)=IST
      GO TO 227
  222 IF(IFC.EQ.0)   GO TO 219
      LOCAF(2)=IST
      GO TO 227
  223 IF(IFF.EQ.0)   GO TO 219
      LOCAF(3)=IST
      GO TO 227
  224 IF(IFE.EQ.0)   GO TO 219
      LOCAF(4)=IST
      GO TO 227
  225 IF(IFER.EQ.0)  GO TO 219
      LOCAF(5)=IST
      GO TO 227
  226 CONTINUE
      LOCAF(6)=IST
  227 IST=IST+NSIG*NTEMP*(NGMAX-NGMIN+1)
  219 CONTINUE
  220 CONTINUE
C
      RETURN
C
  230 FORMAT(1H ,' ##LOCAM## ',11I8)
  231 FORMAT(1H ,' ##LOCAF## ', 6I8)
C
C
      END
