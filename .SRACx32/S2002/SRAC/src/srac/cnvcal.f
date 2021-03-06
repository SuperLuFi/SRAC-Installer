      SUBROUTINE CNVCAL(IFNAME,FINAME,NCOMPF,SIGMA ,P1E   ,NRGNE ,
     1                  PVOL  ,BUFF  ,IBUFF ,CAP   ,ABS   ,NCOMP ,
     2                  LREAC1,LREAC2,
     3                  IOPT4 ,JVX   ,IVX   ,KBVX  ,LVX   ,MVX   ,
     4                  IGMAX ,LBUFF ,ICF   ,IRANG ,LSIGMB,NOUT1 ,
     5                  NOUT2 ,IOFLUX,IDIM  ,MEMORY               )
C
CCNVCAL --- CONVERSION RATIO CALCULATION FOR CITATION ONLY
C
CDEL  INTEGER RGX , MSX , ZNEX , ZDX , WZX
CDEL  PARAMETER ( RGX=100, MSX=211, ZDX=200, ZNEX=1000, WZX=100 )
      INCLUDE  'CITPMINC'
C
C
      DIMENSION IFNAME(1),FINAME(2,MVX),NCOMPF(MVX),
     1          SIGMA(5,IGMAX,LVX),P1E(JVX,IVX,KBVX),
     2          NRGNE(JVX,IVX,KBVX),PVOL(LVX),BUFF(LBUFF),
     3          IBUFF(LBUFF),CAP(5,MVX),ABS(5,MVX),NCOMP(LVX)
      DIMENSION  LREAC1(IOPT4),LREAC2(IOPT4)
C
C
      IBLSB = 1341 + 4*WZX
      IBMES = 33   + 6*RGX + 9*MSX + 2*ZNEX
      IBFLX = 76   + 2*MSX
      IBBUR = 530  + 11*ZDX
C
      LENG  = IBLSB + IBMES + IBFLX + IBBUR
     &      + 26    + 109   + 63    + 141 + 500
C
      REWIND 92
      READ(92)(DUM,I=1,LENG),(((NRGNE(J,I,K),J=1,JVX),I=1,IVX),K=1,KBVX)
     1       ,(PVOL(I),I=1,LVX),(NCOMP(I),I=1,LVX)
      REWIND 92
C
CM    WRITE(6,*) ' ** JVX IVX KBVX LVX MVX : ',JVX,IVX,KBVX,LVX,MVX
CM    WRITE(6,*) ' ** PVOL : ',PVOL
CM    WRITE(6,*) ' ** NRGNE: ',NRGNE
CM    WRITE(6,*) ' ** NCOMP: ',NCOMP
C
      CALL CNVPRE(IFNAME,FINAME,NCOMPF,IOPT4 ,MVX   ,NOUT1  ,
     1            LREAC1,LREAC2,IFNAME)
C
      CALL CNVSIG(FINAME,SIGMA ,NCOMPF,BUFF  ,IBUFF ,IGMAX ,
     1            LBUFF ,NOUT1 ,MEMORY,IRANG ,ICF   ,LSIGMB,
     2            MVX   )
C
      CALL CNVCA1(FINAME,SIGMA ,IOPT4 ,P1E   ,NRGNE ,NCOMPF,
     1            PVOL  ,CAP   ,ABS   ,NCOMP ,JVX   ,IVX   ,
     2            KBVX  ,IGMAX ,LVX   ,MVX   ,IDIM  ,NOUT2 ,
     3            IOFLUX,LREAC1,LREAC2                      )
C
      RETURN
      END
