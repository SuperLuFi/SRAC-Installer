C **********************************************************************
C                         MICREM
C **********************************************************************
      SUBROUTINE MICREM(ICOLP,MAR,IXR,NXR)
C
      CHARACTER*4    CASEID,FILENM
      CHARACTER*4    IDENT(2)
C
      COMMON /MAINC/ IOPT(52)
     1   ,NEFL     ,NETL     ,NEF      ,NET      ,NERF     ,NERT
     2   ,NMAT     ,NETL1    ,BSQ      ,NIN1     ,NIN2     ,NOUT1
     3   ,NOUT2,IT0,NEFL1    ,NEFL2    ,NEFL3    ,NEF1     ,NEF2
     4   ,NEF3     ,ISTEP    ,NSOUC    ,NFIN     ,NFOUT    ,DUMMY1(4)
     5   ,LCNEGF   ,LCNEGT   ,LCNECF   ,LCNECT   ,LCMTNM   ,LCNISO
     6   ,LCTEMP   ,LCXL     ,LCXCDC   ,LCLISO   ,LCIDNT   ,LCDN
     7   ,LCIRES   ,LCIXMC   ,NTOT  ,MAXWOK  ,IPLOT,IRANG,ICF,INITL
     8   ,CASEID(2),TITLE(18)
     9   ,AA(380)
C
      COMMON /PDSPDS/ BUF(540),IFLSW,FILENM(3),ECODE,TEMPRY
      COMMON /WORK  / A(100000)
C
      DIMENSION MAR(1),IXR(1)
C
      WRITE(NOUT2,36)  ISTEP,CASEID,TITLE
C
      IF(IOPT(4).EQ.0) NET  = 0
      IF(IOPT(4).EQ.0) NERT = 0
      NETOT   = NEF  + NET
      NRTOT   = NERF + NERT
      IF(ICOLP.EQ.0) THEN
                     NRTOT = NETOT
                     ENDIF
C
C *** SET NRR WITH CASEVOL MEMBER IN FLUX FILE
C
CKSK  FILENM(1)= 4HFLUX
      FILENM(1)= 'FLUX'
CKSK  FILENM(2)= 4H
      FILENM(2)= '    '
      IDENT(1) = CASEID(1)
CKSK  IDENT(2) = 4HFVOL
      IDENT(2) = 'FVOL'
      NRR      = 0
C
      IF(IRANG.EQ.2) IDENT(2) (1:1) = 'A'
      CALL SEARCH(IDENT,NRR,ISW)
      IF(ISW.EQ.1) THEN
                   WRITE(NOUT1,32) IDENT
                   STOP
                   ENDIF
C
      LOC1  = 1
      LOC2  = LOC1 + NETOT
      LOC3  = LOC2 + NRR
      LOC4  = LOC3 + NRR*NETOT
      LOC5  = LOC4 + NMAT*NETOT
      LOC6  = LOC5 + NMAT*NRTOT
      LOC7  = LOC6 + NXR *NRTOT
      LOC8  = LOC7 + NMAT
      LOC9  = LOC8 + NXR
      LOC10 = LOC9 + 4*NMAT
      LOC11 = LOC10+ NMAT
C
      CALL MICREF(ICOLP,AA(LCMTNM),AA(LCNECF),AA(LCNISO),AA(LCTEMP)
     *  ,AA(LCIXMC),AA(LCLISO),AA(LCIDNT),AA(LCIRES),AA(LCDN),MAR,
     *   IXR,NXR,NRR,NETOT,NRTOT,IDENT,
     *   A(LOC1),A(LOC2),A(LOC3),A(LOC4),A(LOC5),
CKSK *   A(LOC6),A(LOC7),A(LOC8),A(LOC9),A(LOC10),A(LOC11) )
     *   A(LOC6),A(LOC7),A(LOC8),A(LOC9),A(LOC10),A(LOC11),LOC11 )
C
      RETURN
C
   32 FORMAT(' ** MEMBER ',2A4,' NOT FOUND IN FLUX FILE -> ERROR STOP')
   36 FORMAT(//1H1/' *** STEP ',I2,' FOR EFFECTIVE X-SECTION IN'
     * ,/ ' ===CASEID=',2A4,'=TITLE=',18A4,'===')
C
      END
