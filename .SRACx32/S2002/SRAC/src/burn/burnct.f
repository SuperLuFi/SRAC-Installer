      SUBROUTINE BURNCT(NMP   ,NXR   ,VOLZ  ,MAXMT3,
     @                  NISO  ,FLUXMF,MATXRG,VOLM  ,VOLX  ,
     @                  FLUXXF,CC    ,CCB   ,FLUXMB,FLUXXB,
     @                  DNREST,FLUXM1,FLUXX1,WTHVYM,POWMAT,
     @                  SSC1G ,SSF1G ,SSA1G ,S2N1G ,SSP1G ,
     @                  SSC1B ,SSF1B ,SSA1B ,S2N1B ,SSP1B ,
     @                  SIGF  ,SIGA  ,SIGC  ,AFISSM,CFERTM,
     @                  SSCXE5,SSCI35,SSCSM9,SSCPM9,NUCLP ,
     @                  GAM   ,GAMI  ,NBIC  ,PBIC  ,KSTP  ,
     @                  LONG  ,LL    ,IP    ,KP    ,NPN   ,
     @                  PHAI  ,DNOLD ,CCX   ,AII   ,CHAII , III  ,
     @                  LENWRK,A     ,MTNAME,AWORK ,IWORK ,MAXWRK )
C
C
C
      CHARACTER*4   TITLE
      CHARACTER*4   CASEID,IDTEMP
C
      COMMON /MAINC/ IOPT(20),JNFSTL(2),FNFSTL(2),JNTHEL(2),FNTHEL(2)
     1    ,JNEFST(2),FNEFST(2),JNETHE(2),FNETHE(2),JNMACR(2),FNMACR(2)
     2    ,JNMCRS(2),FNMCRS(2),JNEMIC(2),FNEMIC(2),JNFLUX(2),FNFLUX(2)
     3   ,NEFL     ,NETL     ,NEF      ,NET      ,NERF     ,NERT
     4   ,NMAT     ,NETL1    ,BSQ      ,NIN1     ,NIN2     ,NOUT1
     5   ,NOUT2    ,IT0      ,NEFL1    ,NEFL2    ,NEFL3    ,NEF1
     6   ,NEF2     ,NEF3     ,ISTP     ,NSOUC    ,NFIN     ,NFOUT
     7   ,ITYPE    ,IMCEF    ,I79      ,I80
     8   ,LCNEGF   ,LCNEGT   ,LCNECF   ,LCNECT   ,LCMTNM   ,LCNISO
     9   ,LCTEMP   ,LCXL     ,LCXCDC   ,LCLISO   ,LCIDNT   ,LCDN
     A   ,LCIRES   ,LCIXMC   ,NFTOT    ,MEMORY   ,IOPEN    ,IRANG
     B   ,ICF      ,INITL
     C   ,CASEID(2),TITLE(18)
     D   ,II(880)
C
      COMMON /TMPSET/ STND(35),IDTEMP(61),NTDUMY
      COMMON /NEWDAN/ DANNEW(2000)
C
      INCLUDE  'BURNPINC'
C
      COMMON /BURNC1/ IBC(20),NEP,NEP1,IBREST,IBCOLP,NGBN,NGTBN,
     1                IMAXBN,LAPSBN,IBEND,EVTOJ,ABOGA,LASTFP,
     2                IDU235,ST235,NTDEPZ,IBUNIT,IBEDIT,NTISO,
     3                L1,L2,L3,L4,L5,L6,NTNUC,NTFISS,LNMAX,NPAR,
     4                NFIS,NFER,INTSTP,IVOID,NCHA,LCHA,
     5                IDXE35,IDI135,IDSM49,IDPM49,
     6                IFISDN(MXFISS),FISFCT(MXFISS),
     7                IFRTDN(MXFISS),FRTFCT(MXFISS)
CKK9 8               ,DNREPL(MXSTEP,MXREPL,MXDEPL),ISREPL
     8               ,DNREPL(MXSTEP,MXREPL,MXDEPL),ISREPL,ISOREP(MXREPL)
     9               ,ITOTIO
C
      COMMON /BURNC2/ DNNEW(MXNUC,MXDEPL),NSTP(MXSTEP),NREGBN(NGMAX),
     1                MTYP(MXZONE),NDEPZ(MXZONE),LISO(MXZONE),
     2                REACEV(2,MXNUC),IFISS(MXNUC),AMASS(MXNUC),
     3                MATDPL(MXZONE)
C
      CHARACTER*4     NAMFIS,NAMFER,CASBRN,SRACID,STDNUC
      CHARACTER*4     CASINT
      REAL*4          INSCR ,INTCR
C
      COMMON /BURNC3/ NAMFIS(MXFISS),NAMFER(MXFISS),CASBRN,
     1                SRACID(MXNUC) ,STDNUC  ,CASINT
      COMMON /DEPLET/ AKEFF (MXSTEP),AKINF (MXSTEP),
     1                PERIOD(MXSTEP),POWERL(MXSTEP),
     2                DAYS  (MXSTEP),U235F (MXSTEP),
     3                EXPST (MXSTEP),CUMMWD(MXSTEP),
     4                INSCR (MXSTEP),INTCR (MXSTEP),
     5                FLXNRM(MXSTEP),FACNRM(MXSTEP),
     6                FISABS(MXSTEP),FRTCAP(MXSTEP),
     7                FDECAY(MXSTEP),CDECAY(MXSTEP),
     8                POWRZN(MXSTEP,MXDEPL),
     9                EXPSZN(MXSTEP,MXDEPL),
     A                HMINV (MXSTEP,MXDEPL),
     B                DMWZON(MXSTEP,MXDEPL),
     C                IPBURN(MXNUC ,MXDEPL)
C
       REAL*4       SIGF(LAPSBN,NMAT),SIGA(LAPSBN,NMAT)
       REAL*4       SIGC(LAPSBN,NMAT)
       REAL*4       SSCXE5(LAPSBN,NMAT),SSCI35(LAPSBN,NMAT)
       REAL*4       SSCSM9(LAPSBN,NMAT),SSCPM9(LAPSBN,NMAT)
       REAL*4       AFISSM(LAPSBN,NMAT),CFERTM(LAPSBN,NMAT)
C
      INTEGER*4    NISO(NMAT),MATXRG(NMAT)
      REAL*4       FLUXMF(IMAXBN,NMAT),FLUXXF(IMAXBN,NXR)
      REAL*4       VOLX (NXR),VOLM(NMAT)
      REAL*4       CC(IMAXBN,MAXMT3,MXNUC,NMAT)
      REAL*4       WTHVYM(NMAT),POWMAT(NMAT)
C
      REAL*4       CCB(LAPSBN,MAXMT3,MXNUC,NMAT)
      REAL*4       CCX(LAPSBN,MAXMT3,MXNUC,NMAT)
      REAL*4       FLUXMB(LAPSBN,NMAT),FLUXXB(LAPSBN,NXR)
      REAL*4       DNREST(NEP1,NTNUC,NTDEPZ)
      REAL*4       FLUXM1(NMAT)       ,FLUXX1(NXR)
C
      REAL*4       SSC1G(MXNUC,NMAT) ,SSF1G(MXNUC,NMAT)
      REAL*4       SSA1G(MXNUC,NMAT) ,S2N1G(MXNUC,NMAT)
      REAL*4       SSC1B(NTNUC,NTDEPZ),SSF1B(NTNUC,NTDEPZ)
      REAL*4       SSA1B(NTNUC,NTDEPZ),S2N1B(NTNUC,NTDEPZ)
      REAL*4       SSP1G(MXNUC,NMAT) ,SSP1B(NTNUC,NTDEPZ)
      DIMENSION    VOLZ(NMP)
C
      DIMENSION         NUCLP (NPAR,NTNUC)
      DIMENSION         GAM   (NTFISS,NTNUC)
      DIMENSION         GAMI  (NTFISS,NTNUC)
C
      DIMENSION         NBIC  (NPAR  ,NTNUC)
      DIMENSION         PBIC  (NPAR  ,NTNUC)
      DIMENSION         KSTP  (NCHA  ,NTNUC)
      DIMENSION         LONG  (NCHA  ,NTNUC)
      DIMENSION         LL    (NCHA  ,NTNUC)
      DIMENSION         IP    (LCHA  ,NCHA  ,NTNUC)
      DIMENSION         KP    (LCHA  ,NCHA  ,NTNUC)
      DIMENSION         NPN   (NPAR  ,NTNUC)
      DIMENSION         PHAI  (NPAR  ,NTNUC)
      DIMENSION         DNOLD (MXNUC,MXDEPL)
C
      REAL*4       A   (1)
      CHARACTER*8  MTNAME(NMAT)
C
CKK9  DATA    ITOTIO  / 0 /
C
C *** LOCAL DIMENSION
C
      DIMENSION    FLUX(MXDEPL)
      DIMENSION    PHIP(MXDEPL),VOLB(MXDEPL),IBZONE(MXDEPL)
      DIMENSION    POWZN1(MXDEPL),POWZN2(MXDEPL)
      DIMENSION    DELMWD(MXDEPL)
      DIMENSION    NUCL(MXNUC),NCH(MXNUC),RAMDA(MXNUC),FACT2N(MXNUC)
C ***  LOCAL DIMESION
       REAL*4       YLDXE5(MXZONE),YLDI35(MXZONE)
       REAL*4       YLDSM9(MXZONE),YLDPM9(MXZONE)
       REAL*4       ABSRAT(MXZONE),GAMAVG(MXZONE)
       REAL*4       WTHVYW(MXZONE),WTHVY0(MXZONE)
C
      CHARACTER*8  MEMBER
C
      REAL*4       AII (LENWRK),AWORK(MAXWRK)
      INTEGER*4    III (LENWRK),IWORK(MAXWRK)
      CHARACTER*4  CHAII(LENWRK)
C
C
C  **** START OF PROCESS
C
      J79    = I79+1
      K79    = I79+2
      WRITE(NOUT1,630) J79
      WRITE(NOUT2,635) J79
C
C *** READ X-SECTION TO FT52F001
C
      REWIND 52
      READ(52)
      READ(52)  NISO,FLUXMF,MATXRG,VOLM,VOLX,FLUXXF
      READ(52)  CC
      REWIND 52
C
C ----------- CHECK WRITE ----------------------------------------------
      IF(IBEDIT.GE.2)  THEN
                       WRITE(NOUT2,650) (VOLZ(I),I=1,NMP)
                       ENDIF
C-----------------------------------------------------------------------
C
C ****** READ EFFECTIVE MICRO CROSS SECTION AND FLUX ***********
C
      CALL  CLEA( DNOLD , MXNUC *MXDEPL , 0.0 )
      CALL  CLEA( FLUX  ,    MXDEPL     , 0.0 )
      CALL  CLEA( PHIP  ,    MXDEPL     , 0.0 )
      CALL  CLEA( VOLB  ,    MXDEPL     , 0.0 )
      CALL  CLEA( DELMWD,    MXDEPL     , 0.0 )
      CALL ICLEA( IBZONE,    MXDEPL     , 0   )
C
C *** LOOP OF MATERIAL ; IF X-REGION NO OF FUEL IS ZERO => NO BURNUP
C
      MTYPAV   = 0
      IFUEL    = 0
      DO 40  M = 1,NMAT
      KK       = NISO  (M)
      NOXREG   = MATXRG(M)
      IF(KK*NOXREG.GT.0) THEN
                         KDEPZ    = MATDPL(M)
         IF(KDEPZ.GT.0)  THEN
                         VOLB  (KDEPZ) = VOLM(M)
                         IBZONE(KDEPZ) = 1
                         MTYPAV        = MTYPAV + MTYP(M)
                         IFUEL         = IFUEL  + 1
                         ENDIF
                         ENDIF
   40 CONTINUE
      IF(IFUEL.GT.0) THEN
                     MTYPAV = MTYPAV/IFUEL
                     ELSE
         WRITE(NOUT1,*) ' ** WARNING : THIS PROBLEM HAS NO FUEL !! '
         WRITE(NOUT1,*) ' ** SO BURNUP CALCULATION WILL BE SKIPPED.'
         WRITE(NOUT2,*) ' ** WARNING : THIS PROBLEM HAS NO FUEL !! '
         WRITE(NOUT2,*) ' ** SO BURNUP CALCULATION WILL BE SKIPPED.'
C
                     IBEND = 1
                     RETURN
                     ENDIF
C
       IF(MTYPAV.EQ.2.AND.J79.EQ.1.AND.IVOID.EQ.0) THEN
C
           IF(IBC(2).LE.2.OR.IBC(3).NE.3) THEN
       WRITE(NOUT1,*) ' ** WARNING : THIS BURNUP PROBREM HAS NO FUEL.'
       WRITE(NOUT1,*) ' ** FOR THIS CASE , IBC2>2 & IBC3=3 AND USER MUST
     1 BE INPUT INITIAL FLUX LEVEL (N/CM/CM/SEC) ]] '
       WRITE(NOUT2,*) ' ** WARNING : THIS BURNUP PROBREM HAS NO FUEL.'
       WRITE(NOUT2,*) ' ** FOR THIS CASE , IBC2>2 & IBC3=3 AND USER MUST
     1 BE INPUT INITIAL FLUX LEVEL (N/CM/CM/SEC) ]] '
                                 STOP 970
C
                                 ELSE
                       IF(FLXNRM(1).LE.0.0) THEN
       WRITE(NOUT1,*) ' ** ERROR STOP  : INPUT FLUX LEVEL IS ZERO !!'
       WRITE(NOUT2,*) ' ** ERROR STOP  : INPUT FLUX LEVEL IS ZERO !!'
                                             STOP 980
                                             ENDIF
                                 ENDIF
                       ENDIF
C
C *** GET COLLAPSBND FLUX & EFFCTIVE MICROSCOPIC CROSS SECTION
C *** AND CALCULATEE ONE-GROUP EFFCTIVE MICROSCOPIC CROSS SECTION
C
      CALL  BURNXS( IMAXBN,LAPSBN ,NMAT  ,NXR   ,MAXMT3 ,MXNUC,
     1              NREGBN,NISO   ,MATXRG,VOLM  ,VOLX   ,
     2              FLUXMF,FLUXXF ,FLUXMB,FLUXXB,FLUXM1 ,FLUXX1,
     3              CC    ,CCB    ,CCX   ,IPBURN,NTDEPZ ,MATDPL,
     4              SSC1G ,SSF1G  ,SSA1G ,S2N1G ,SSP1G  ,NTNUC )
C
C   1. SET ONE-GROUP MICROSCOPIC X-SECTION FOR BURNCL ROUTINE
C      & SET HEAVY METAL WEIGHT
C   2. SET ABSOLUTE FLUX LEVEL AND MATERIAL-WISE POWER
C
      NBOPT   =  0
      POWNOW  = POWERL(J79)
      IF(POWNOW.LE.0.0)  THEN
                         NBOPT = 1
                         ENDIF
C
      CALL  MEMOVE ( DNNEW , DNOLD , MXNUC*MXDEPL )
C
       IF(IBC(3).EQ.3.AND.J79.GT.1) THEN
                                    FLXNRM(J79) = FLXNRM(1)
                                    ENDIF
C
       COEFLX  = 1.0000
C
       CALL    BURNRM ( IBC(3) , NMAT   , MXNUC , NTNUC  , NTDEPZ ,
     1                  MATDPL , NISO   , VOLM  , FLUXM1 ,
     2                  IPBURN , PHIP   , POWMAT, DNOLD  ,
     3                  SSC1G  , SSF1G  , SSA1G , S2N1G  , SSP1G  ,
     4                  SSC1B  , SSF1B  , SSA1B , S2N1B  , SSP1B  ,
     5                  POWNOW , FLXNRM(J79)    , EVTOJ  , REACEV ,
     6                  COEFLX , NTFISS , ABOGA , AMASS  , WTHVYM ,
     7                  J79    , NOUT2  , IBZONE, IBEDIT )
C
      IF(IBEDIT.GE.1) THEN
                      DO 20 M = 1 , NTDEPZ
                      WRITE(NOUT2,672) M
                      DO 20 I = 1 , NTNUC
                      WRITE(NOUT2,673) I,SRACID(I),SSA1B(I,M),
     1                       SSF1B(I,M),SSC1B(I,M),S2N1B(I,M)
   20                 CONTINUE
                      ENDIF
C
      TWTHVY      = 0.0
      DO 30    M  = 1 , NMAT
      MPOS        = MATDPL(M)
      IF(MPOS.GT.0) THEN
                    IF(IBZONE(MPOS).GT.0) THEN
                    HMINV (J79,MPOS) = WTHVYM(M)
                    TWTHVY           = TWTHVY   + VOLM(M)*HMINV(1,MPOS)
                    ENDIF
                    ENDIF
   30 CONTINUE
C
CKSK  CALL CLOCKM(KT1)
      CALL UCLCKM(KT1)
      FACPOW = 1.000
      DELDAY = 0.0
C
C **** CASE FOR NORMAL BURNUP CLACULATION
C
      IF(IVOID.EQ.0) THEN
                     POWERL(J79) = POWNOW
C
      CALL  BURNTM ( J79    , IBC(2) , IBEDIT , IDU235 ,NMAT   ,
     1               NTNUC  , MXDEPL , MATDPL , DNOLD  ,SSA1B  ,
     2               MXSTEP , POWNOW , TWTHVY , ST235  ,PHIP   ,
     3               PERIOD , DAYS   , U235F  , EXPST  ,CUMMWD ,
     4               DELDAY , VOLM   , MATXRG , MXNUC  ,NOUT2  )
C
      DELT    = DELDAY
      ITD     = NSTP(J79)
      IDBG    = IBEDIT - 2
C
C  *** READ CHAIN DATA
C
CKSK  MEMBER = 8HNUCL
      MEMBER = 'NUCL    '
      CALL BURNRW( MEMBER , NUCL  , NTNUC , 1 , 1 , NTNUC , 1 , 1 ,
     1             'READ'  )
CKSK  MEMBER = 8HNCH
      MEMBER = 'NCH     '
      CALL BURNRW( MEMBER , NCH   , NTNUC , 1 , 1 , NTNUC , 1 , 1 ,
     1             'READ'  )
CKSK  MEMBER = 8HRAMDA
      MEMBER = 'RAMDA   '
      CALL BURNRW( MEMBER , RAMDA , NTNUC , 1 , 1 , NTNUC , 1 , 1 ,
     1             'READ'  )
CKSK  MEMBER = 8HFACT2N
      MEMBER = 'FACT2N  '
      CALL BURNRW( MEMBER , FACT2N, NTNUC , 1 , 1 , NTNUC , 1 , 1 ,
     1             'READ'  )
CKSK  MEMBER = 8HGAM
      MEMBER = 'GAM     '
      CALL BURNRW( MEMBER , GAM   , NTFISS, NTNUC ,1,NTFISS,NTNUC,1 ,
     1             'READ'  )
CKSK  MEMBER = 8HNUCLP
      MEMBER = 'NUCLP   '
      CALL BURNRW( MEMBER , NUCLP , NPAR  , NTNUC ,1,NPAR  ,NTNUC,1 ,
     1             'READ'  )
CKSK  MEMBER = 8HNBIC
      MEMBER = 'NBIC    '
      CALL BURNRW( MEMBER , NBIC  , NPAR  , NTNUC ,1,NPAR  ,NTNUC,1 ,
     1             'READ'  )
CKSK  MEMBER = 8HPBIC
      MEMBER = 'PBIC    '
      CALL BURNRW( MEMBER , PBIC  , NPAR  , NTNUC ,1,NPAR  ,NTNUC,1 ,
     1             'READ'  )
C
C
C
          IF(IBC(3).EQ.3) THEN
               CALL BURNCF(PHIP  ,IBZONE ,VOLB  ,DELT  ,NTDEPZ,NBOPT  ,
     1                     ITD   ,MXNUC  ,NTNUC ,NTFISS,NPAR  ,EVTOJ  ,
     2                     REACEV,DNNEW  ,DNOLD ,SSF1B ,SSC1B ,S2N1B  ,
     3                     DELMWD,IDBG   ,NOUT2 ,LCHA  ,NCHA  ,IPBURN ,
     4                     NUCLP ,GAM    ,GAMI  ,NBIC  ,PBIC  ,KSTP   ,
     5                     LONG  ,LL     ,IP    ,KP    ,NPN   ,PHAI   ,
     6                     IBC(2),ST235  ,PERIOD(J79)  ,IDU235,FACPOW ,
     7                     POWZN1,POWZN2 ,NUCL  ,NCH   ,RAMDA ,FACT2N )
               ELSE
               CALL BURNCL(PHIP  ,IBZONE ,VOLB  ,DELT  ,NTDEPZ,NBOPT  ,
     1                     ITD   ,MXNUC  ,NTNUC ,NTFISS,NPAR  ,EVTOJ  ,
     2                     REACEV,DNNEW  ,DNOLD ,SSF1B ,SSC1B ,S2N1B  ,
     3                     DELMWD,IDBG   ,NOUT2 ,LCHA  ,NCHA  ,IPBURN ,
     4                     NUCLP ,GAM    ,GAMI  ,NBIC  ,PBIC  ,KSTP   ,
     5                     LONG  ,LL     ,IP    ,KP    ,NPN   ,PHAI   ,
     6                     IBC(2),ST235  ,PERIOD(J79)  ,IDU235,FACPOW ,
     7                     POWZN1,POWZN2 ,NUCL  ,NCH   ,RAMDA ,FACT2N )
               ENDIF
C
               FACNRM(J79) = FACPOW
C
               IF(IBEDIT.GT.1) THEN
                         WRITE(NOUT2,*) ' **J79 DELDAY DELT FACPOW:',
     1                                      J79,DELDAY,DELT,FACPOW
                               ENDIF
C
               DAYS(K79)   = DAYS(J79) + DELT
C
               SUMMWD      = 0.0
               DO 50    M  = 1 , NTDEPZ
               SUMMWD      = SUMMWD    + DELMWD(M)
   50          CONTINUE
C
               CUMMWD(K79) = CUMMWD(J79) + SUMMWD*1.0000E-6
               IF(TWTHVY.GT.0.0)  EXPST (K79) = CUMMWD(K79) / TWTHVY
C
            CALL  CLEA (  POWZN1 , MXDEPL , 0.0 )
            CALL  CLEA (  POWZN2 , MXDEPL , 0.0 )
C
            DO 60   M  = 1 , NMAT
            MPOS       = MATDPL(M)
            IF(MPOS.GT.0) THEN
            DMWZON(K79,MPOS) = DMWZON(J79,MPOS)+DELMWD(MPOS)*1.00000E-6
            SAVEWT           = HMINV(1,MPOS)
            IF(SAVEWT.LE.0.0) SAVEWT = 1.00000
            EXPSZN(K79,MPOS) = DMWZON(K79,MPOS)/SAVEWT/VOLM(M)
            POWZN1(MPOS)     = DMWZON(K79,MPOS)
            POWZN2(MPOS)     = EXPSZN(K79,MPOS)
                       ENDIF
   60       CONTINUE
C***** REPLACE IF IBC(9)>0 *********************************************
      IF(ISREPL.GT.0) THEN
CKK9                  DO  66   I = 1 , NTNUC
                      DO  67   I = 1 , ISREPL
                      ISOPOS     = ISOREP(I)
            IF(ISOPOS.GT.0.AND.ISOPOS.LE.NTNUC) THEN
                      DO  66   M = 1 , NTDEPZ
                      SAVE       = DNREPL(K79,I,M)
CKK9                  IF(SAVE.GE.0.0)  DNNEW(I,M) = SAVE
                      IF(SAVE.GE.0.0)  DNNEW(ISOPOS,M)= SAVE
   66                 CONTINUE
                                                ENDIF
   67                 CONTINUE
                      ENDIF
            ENDIF
C
C **** CASE FOR BRANCHING BURNUP CLACULATION
C
      IF(IVOID.EQ.1) THEN
CKSK                 MEMBER = 8HRAMDA
                     MEMBER = 'RAMDA   '
                     CALL BURNRW( MEMBER , RAMDA , NTNUC , 1 , 1 ,
     1                            NTNUC  , 1     , 1     , 'READ'  )
CKSK                 MEMBER = 8HGAM
                     MEMBER = 'GAM     '
                     CALL BURNRW( MEMBER , GAM   , NTFISS, NTNUC , 1 ,
     1                            NTFISS , NTNUC , 1     , 'READ'  )
CKSK                 MEMBER =8HDNREST
                     MEMBER ='DNREST  '
               CALL  BURNRW( MEMBER , DNREST , NEP1  , NTNUC  ,
     1                       NTDEPZ , NEP1   , NTNUC , NTDEPZ ,'READ')
C
                     DO 165   M = 1 , NTDEPZ
                     DO 165   I = 1 , NTNUC
                     DNNEW(I,M) = DNREST(K79,I,M)
  165                CONTINUE
C***** REPLACE IF IBC(9)>0 *********************************************
      IF(ISREPL.GT.0) THEN
CKK9                  DO 166   I = 1 , NTNUC
                      DO 167   I = 1 , ISREPL
                      ISOPOS     = ISOREP(I)
            IF(ISOPOS.GT.0.AND.ISOPOS.LE.NTNUC) THEN
                      DO 166   M = 1 , NTDEPZ
                      SAVE       = DNREPL(K79,I,M)
CKK9                  IF(SAVE.GE.0.0)  DNNEW(I,M) = SAVE
                      IF(SAVE.GE.0.0)  DNNEW(ISOPOS,M) = SAVE
  166                 CONTINUE
                                                ENDIF
  167                 CONTINUE
                      ENDIF
C
                     CALL  CLEA (  POWZN1 , MXDEPL , 0.0 )
                     CALL  CLEA (  POWZN2 , MXDEPL , 0.0 )
C
                     DO 170  MN = 1 , NTDEPZ
                     POWZN1(MN) = DMWZON(K79,MN)
                     POWZN2(MN) = EXPSZN(K79,MN)
  170                CONTINUE
                     FACNRM(J79) = FACPOW
                     ENDIF
C
CKSK  CALL CLOCKM(KT2)
      CALL UCLCKM(KT2)
C
      TT   = FLOAT(KT2-KT1)/1000.0
      WRITE(NOUT2,670) J79,TT
C
C ****  BURNUP STEP-WISE EDIT
C
       CALL BURNED ( J79    , IBEDIT , LAPSBN , MATDPL , MXNUC ,
     1               NMAT   , NISO   , VOLM   , FLUXMB , MTYP   ,
     2               NTNUC  , NTDEPZ , DNOLD  , DNNEW  , IDU235 ,
     3               POWNOW , EVTOJ  , REACEV , MATXRG , SRACID ,
     4               NTFISS , MAXMT3 , ABOGA  , AMASS  , GAM    ,
     5               ST235  , U235F(K79) , DAYS(K79)   , EXPST(K79) ,
     6               AKEFF (J79)     , AKINF(J79)      ,
     7               IDXE35 , IDI135 , IDSM49 , IDPM49 ,
     8               POWMAT , NFIS   , NFER   , NAMFIS , NAMFER ,
     9               IFISDN , IFRTDN , FISFCT , FRTFCT , LISO   ,
     A               SSCXE5 , SSCI35 , SSCSM9 , SSCPM9 , CCB    ,CCX  ,
     B               A(L4+1), COEFLX , AFISSM , CFERTM , FACNRM(J79),
     C               SIGC   , SIGF   , SIGA   , RAMDA  ,
     D               SSA1B  , AWORK  , IWORK  , MAXWRK , MEMBER ,
     E               CASEID , FLUXM1 , CUMMWD(K79) , FLXNRM(J79),
     F               POWZN1 , POWZN2 , NOUT2  , MXDEPL , MXZONE ,
     G               YLDXE5 , YLDI35 , YLDSM9 , YLDPM9 ,
     H               ABSRAT , GAMAVG , WTHVYW , WTHVY0 ,
     I               NXR    , VOLX   )
C
C ***** COPY NEW DENSITY
C ***** UPDATE I79 FOR BURNUP STEP NUMBER
C
      CALL   BURNXT( J79    ,  NMAT   , MATDPL , LISO   , IDTEMP(J79) ,
     1               NTNUC  ,  NTDEPZ , A(L4+1), DNNEW  , MTNAME ,
     2               SRACID ,  IBEND  , NEP    , I79    , NOUT2  ,
     3               IPBURN ,  MATXRG , MXNUC  )
C
C ***** CHECK I/O LIMIT
C
C
       CALL UEXCP(LIMIT,IONOW)
       ISAVE = IBEND
       IF(ISAVE.EQ.0) THEN
                      NEXTIO = IONOW + (IONOW-ITOTIO)
                      ITOTIO = IONOW
CDEL   WRITE(NOUT2,* ) ' ** I79 ITOTIO NEXTIO : ',I79,ITOTIO,NEXTIO
                      IF(NEXTIO.LT.LIMIT.AND.I79.LT.NEP)  GO TO 280
                      IBEND  = 1
                      ENDIF
C
CDEL   WRITE(NOUT2,* ) ' ** I79 ITOTIO (B-END): ',I79,ITOTIO
       ITOTIO = IONOW
       NEP    = I79
       NEP1   = NEP + 1
C
C *** EDIT FINAL EDIT
C
      LOC01    = 1
C ----VLDEPX
      LOC02    = LOC01  +  NXR
C ----GAMAV
      LOC03    = LOC02  +  NEP1*NTDEPZ
C ----YDXE
      LOC04    = LOC03  +  NEP1*NTDEPZ
C ----YDIO
      LOC05    = LOC04  +  NEP1*NTDEPZ
C ----YDSM
      LOC06    = LOC05  +  NEP1*NTDEPZ
C ----YDPM
      LOC07    = LOC06  +  NEP1*NTDEPZ
C ----DNSITY
      LOC08    = LOC07  +  NEP1*NTNUC*NTDEPZ
C ----SIGXE
      LOC09    = LOC08  +  NEP1*LAPSBN*NTDEPZ
C ----SIGIO
      LOC10    = LOC09  +  NEP1*LAPSBN*NTDEPZ
C ----SIGSM
      LOC11    = LOC10  +  NEP1*LAPSBN*NTDEPZ
C ----SIGPM
      LOC12    = LOC11  +  NEP1*LAPSBN*NTDEPZ
C ----MTYPX
      LOC13    = LOC12  +  NXR
C ----POWRX
      LOC14    = LOC13  +  NEP1*NXR
C ----EXPSX
      LOC15    = LOC14  +  NEP1*NXR
C ----U235FX
      LOC16    = LOC15  +  NEP1*NXR
C ----HINVX
      LOC17    = LOC16  +  NEP1*NXR
C ----GAMAXV
      LOC18    = LOC17  +  NEP1*NXR
C ----YDXEX
      LOC19    = LOC18  +  NEP1*NXR
C ----YDIOX
      LOC20    = LOC19  +  NEP1*NXR
C ----YDSMX
      LOC21    = LOC20  +  NEP1*NXR
C ----YDPMX
      LOC22    = LOC21  +  NEP1*NXR
C ----DENSX
      LOC23    = LOC22  +  NEP1*NTNUC*NXR
C ----SIGXEX
      LOC24    = LOC23  +  NEP1*LAPSBN*NXR
C ----SIGIOX
      LOC25    = LOC24  +  NEP1*LAPSBN*NXR
C ----SIGSMX
      LOC26    = LOC25  +  NEP1*LAPSBN*NXR
C ----SIGPMX
      LOC27    = LOC26  +  NEP1*LAPSBN*NXR
C ----AFISSX
      LOC28    = LOC27  +  NEP1*LAPSBN*NXR
C ----CFERTX
      LOC29    = LOC28  +  NEP1*LAPSBN*NXR
C ----FLUXX
      LOC30    = LOC29  +  NEP1*LAPSBN*NXR
C ----AFISSM
      LOC31    = LOC30  +  NEP1*LAPSBN*NTDEPZ
C ----CFERTM
      LOC32    = LOC31  +  NEP1*LAPSBN*NTDEPZ
C ----FLUXMM
      LOC33    = LOC32  +  NEP1*LAPSBN*NTDEPZ
C ----SIGA
      LOC34    = LOC33  +  NEP1*LAPSBN*NMAT
C ----SIGF
      LOC35    = LOC34  +  NEP1*LAPSBN*NMAT
C ----ABSRAT
      LOC36    = LOC35  +  NEP1*NMAT
C ----ABSMAT
      LOC37    = LOC36  +  NEP1*NTNUC*NTDEPZ
C ----ABSXRG
      LLAST    = LOC37  +  NEP1*NTNUC*NXR
      MXWORK   = LENWRK -  LLAST + 1
C
      IF(IBEDIT.GE.2) THEN
      WRITE(NOUT2,*) ' *** MEMORY LOACTION FOR BURNCT ROUTINE *** '
      WRITE(NOUT2,*) ' ============================================== '
      WRITE(NOUT2,7) LOC01,LOC02,LOC03,LOC04,LOC05,
     1               LOC06,LOC07,LOC08,LOC09,LOC10,
     2   LOC11,LOC12,LOC13,LOC14,LOC15,LOC16,LOC17,LOC18,LOC19,LOC20,
     3   LOC21,LOC22,LOC23,LOC24,LOC25,LOC26,LOC27,LOC28,LOC29,LOC30,
     4   LOC31,LOC32,LOC33,LOC34,LOC35,LOC36,LOC37
      WRITE(NOUT2,*) ' ============================================== '
      WRITE(NOUT2,*) ' ** LENWRK LLAST MXWORK : ',LENWRK,LLAST,MXWORK
                      ENDIF
C
    7 FORMAT(1H ,5X,10I10)
C
      LENSMY  = 30 + 3*NTDEPZ + NTNUC  + 9*NEP1 + 8*NEP1*NTDEPZ
     1             + NEP1*NTNUC*NTDEPZ + 4*NEP1*NTDEPZ*LAPSBN   + 10000
C
      IF(MXWORK.LT.LENSMY) THEN
         WRITE(NOUT1,*) ' ** ERROR STOP AT SUBROUTINE(BURNCT) ]]] '
         WRITE(NOUT1,*) ' ** WORK DIMENSION IS INSUFFICIENT   ]]] '
         WRITE(NOUT1,*) ' ** REQUESTED IS ',LLAST+LENSMY,' WORDS. '
         WRITE(NOUT1,*) ' ** BUT RESERVED ',MXWORK,' WORDS. '
         WRITE(NOUT2,*) ' ** ERROR STOP AT SUBROUTINE(BURNCT) ]]] '
         WRITE(NOUT2,*) ' ** WORK DIMENSION IS INSUFFICIENT   ]]] '
         WRITE(NOUT2,*) ' ** REQUESTED IS ',LLAST+LENSMY,' WORDS. '
         WRITE(NOUT2,*) ' ** BUT RESERVED ',MXWORK,' WORDS. '
         STOP 999
         ENDIF
C
       CALL  CLEA ( AII, LLAST , 0.0 )
       NTEMP  = 35
C
       CALL BURNSM ( NEP1   , IBEDIT , LAPSBN , MATDPL , NMAT   ,
     1               NXR    , VOLM   , VOLX   , TITLE  , CASEID ,
     2               NTNUC  , NTDEPZ , MATXRG , IDU235 ,
     3               IDXE35 , IDI135 , IDSM49 , IDPM49 , STDNUC ,
     4               NGTBN  , NOUT1  , NOUT2  , IDTEMP , NTEMP  ,
     5               MTYP   , SRACID ,
     6               AII(LOC01) , AII(LOC02) , AII(LOC03) , AII(LOC04) ,
     7               AII(LOC05) , AII(LOC06) , AII(LOC07) , AII(LOC08) ,
     8               AII(LOC09) , AII(LOC10) , AII(LOC11) , AII(LOC12) ,
     9               AII(LOC13) , AII(LOC14) , AII(LOC15) , AII(LOC16) ,
     A               AII(LOC17) , AII(LOC18) , AII(LOC19) , AII(LOC20) ,
     B               AII(LOC21) , AII(LOC22) , AII(LOC23) , AII(LOC24) ,
     C               AII(LOC25) , AII(LOC26) , AII(LOC27) , AII(LOC28) ,
     D               AII(LOC29) , AII(LOC30) , AII(LOC31) , AII(LOC32) ,
     E               AII(LOC33) , AII(LOC34) , AII(LOC35) , AII(LOC36) ,
     F               AII(LOC37) , AII(LLAST) , III(LLAST) ,CHAII(LLAST),
     G               MXWORK     , NTFISS     , LASTFP     , MTNAME  )
C
C-----------------------------------------------------------------------
C
  630 FORMAT(1H0,'*** ENTER BURNUP (TIME STEP=',I2,') ***')
  635 FORMAT(1H1,//50X,'BURNUP (TIME STEP=',I2,')'//)
  650 FORMAT('0  VOLUME OF EACH MATERIAL'/(1P10E13.5))
  670 FORMAT(//1H0,17X,5(1H*),2X,'CURRENT DENSITY --- TIME STEP=',I2,
     1      2X,'( CPU TIME FOR DEPLETION CALCULATION =',E12.5,' SEC.)',
     2      2X,5(1H*)//)
  672 FORMAT(//1H0,17X,5(1H*),2X,'CURRENT 1-G EFFECTIVE MICRO. X-SECTION
     1 OF DEPLETING ZONE ',I2,2X,5(1H*)//)
  673 FORMAT('  ID=',I2,2X,A4,' SIGA=',1PE12.5,' SIGF=',E12.5,' SIGC=',
     *       E12.5,' SIGN2N=',E12.5       )
C
C *** END OF PROCESS
C
  280 CONTINUE
      RETURN
      END
