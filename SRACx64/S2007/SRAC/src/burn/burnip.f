C***********************************************************************
      SUBROUTINE BURNIP(NMP   ,MATD  ,VOLZ  ,NTNUC0,NTFIS0,NPAR0,LEMORY,
     1                  NUCLP ,GAM   ,NBIC  ,PBIC  ,IDENT ,
     2                  IRES  ,IXMICR,DN    ,DANCF ,DNREST,
     3                  IARRAY,RARRAY,CARRAY,AII   ,CHAII ,NOWSTP,LCHA0,
     4                  MATXRG,NISOW ,MAR   ,IXR   ,NRR   ,MKBNUP )
C***********************************************************************
C
      CHARACTER*4    CASEID,TITLE,IDTEMP
CKUNI
      INCLUDE  'MATDTINC'
CEND
      COMMON /MAINC/ IOPT(36)
     1   ,IUPSCT   ,IBSPCT   ,ISCT     ,IFIXS    ,ICTOT    ,ICOND
     2   ,IP1C     ,IFF1     ,LCXIWT   ,IBKSCH   ,MXINP2   ,IDUM45(5)
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
     D   ,II(1880)
C
CKUNI COMMON  /NEWDAN/ DANNEW(2000)
      COMMON  /NEWDAN/ DANNEW(MXLISO)
C
      COMMON  /TMPSET/ STND(35),IDTEMP(61),NTDUMY
C
      COMMON /PCOWK5/ IBURN,KEEPIJ,IPCNT,NXRB
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
CKUNI
      CHARACTER*8     IHOL
      COMMON /BURNC4/ IHOL(MXNUC)
CEND
C
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
      CHARACTER*4   FILENM
      CHARACTER*12  DDNAME
      COMMON /PDSPDS/ DDNAME(125),IST0(15),IRW(15),IOS(35),NC(5,20),
     &                IFLSW,FILENM(3),ECODE,TEMP
C
      INTEGER*4    MATD(NMP),MATXRG(NMAT),MAR(NRR),IXR(NRR)
      REAL*4       VOLZ(NMP)
C
      INTEGER*4             IARRAY(LEMORY)
      REAL*4       AII(1)  ,RARRAY(LEMORY)
      CHARACTER*4  CHAII(1),CARRAY(LEMORY)
C
      DIMENSION    NUCLP(NPAR0,NTNUC0),GAM (NTFIS0,NTNUC0)
      DIMENSION    NBIC (NPAR0,NTNUC0),PBIC(NPAR0 ,NTNUC0)
C
      CHARACTER*4  IDENT (2,MXNUC,NMAT)
      INTEGER*4    IRES  (MXNUC,NMAT),IXMICR(MXNUC,NMAT)
      REAL*4       DN    (MXNUC,NMAT), DANCF(MXNUC,NMAT)
      REAL*4       DNREST(MXSTEP,MXNUC,MXZONE)
C@ADD
      INTEGER*4    IBTYPE(MXZONE)
CKUNI
      CHARACTER*8  NMYLD1,NMYLD2
C
C *** LOCAL DEMENSION AFTER THIS LINE
C
      CHARACTER*4  NMREPL(MXREPL)
      REAL*4       DNWORK(MXNUC)
C
      CHARACTER*4  NAMMIC(MXNUC)
      CHARACTER*4  JDENT(2,MXNUC)
      INTEGER*4    JRES (MXNUC),JXMICR(MXNUC)
      REAL*4       JDN  (MXNUC),JDANCF(MXNUC),DC(MXZONE)
C
      INTEGER*4    NISO(MXZONE),IBPASS(MXNUC),NISOW(NMAT)
      DIMENSION    DUM(50),IDUM(50),VOLM(MXZONE)
C
CKUNI CHARACTER*8  IHOL(MXNUC),IHOLF(MXNUC),IHOLW(MXNUC)
      CHARACTER*8  IHOLF(MXNUC),IHOLW(MXNUC)
      CHARACTER*8  KDENT,TUNIT,IDREAC,MEMBER
      CHARACTER*80 LINE
C
      DIMENSION    NUCL(MXNUC) ,NCODE(MXNUC)  ,FACT2N(MXNUC)
      DIMENSION    NCH (MXNUC) ,YIELDW(MXNUC) ,RAMDA (MXNUC)
      DIMENSION    IIUSE(MXNUC),IRESSB(MXNUC) ,NOL   (MXNUC)
C***********************************************************************
C
C  *** START OF PROCESS
C
      IF(NMP.GT.MXZONE) THEN
         WRITE(NOUT1,*) ' ** ERROR STOP OVER MAX. ZONE NO !!! '
         WRITE(NOUT2,*) ' ** ERROR STOP OVER MAX. ZONE NO !!! '
         STOP 888
         ENDIF
C
C ********** ZERO CLEAR ********
C
      IMCEF  = 1
      IBEND  = 0
      LASTFP = 0
      IVOID  = 0
      MTREPL = 0
      L0     = NEF + NET + NERF + NERT
C
      LENG1  = MXNUC*MXDEPL + MXSTEP    + MXZONE*4   + MXNUC*4  + NGMAX
      LENG2  = MXSTEP*(16  + MXDEPL*4 ) + MXNUC*MXDEPL
      LENG3  = MXNUC*MXDEPL
      LENG4  = MXSTEP*MXNUC*MXZONE
C
      CALL  CLEA( DNNEW , LENG1 , 0.0 )
      CALL  CLEA( AKEFF , LENG2 , 0.0 )
      CALL  CLEA( DNREST, LENG4 , 0.0 )
      CALL ICLEA( IPBURN, LENG3 , 0   )
      CALL ICLEA( NREGBN, NGMAX , 0   )
      CALL ICLEA( NISO  , MXZONE, 0   )
      CALL  CLEA( VOLM  , MXZONE, 0.0 )
      CALL ICLEA( NISOW , NMAT  , 0   )
      CALL  CLEA( RAMDA , MXNUC , 0.0 )
      CALL  CLEA( FACT2N, MXNUC , 0.0 )
      CALL ICLEA( NUCL  , MXNUC , 0   )
      CALL ICLEA( NCH   , MXNUC , 0   )
      CALL ICLEA( NOL   , MXNUC , 0   )
      CALL ICLEA( MATXRG, NMAT  , 0   )
C@ADD
      CALL ICLEA( IBTYPE , MXZONE , 0 )
C
      ISREPL    = IBC(9)
      IF(ISREPL.GT.MXREPL) THEN
            WRITE(NOUT1,*) ' ** INPUT ERROR AT SUB(BURNIP) !!! '
            WRITE(NOUT1,*) ' ** IBC9 IS GREATER THAN MXREPL(',MXREPL,')'
            WRITE(NOUT2,*) ' ** INPUT ERROR AT SUB(BURNIP) !!! '
            WRITE(NOUT2,*) ' ** IBC9 IS GREATER THAN MXREPL(',MXREPL,')'
CKUNI
            STOP 777
CEND
            ENDIF
      LENG5     = MXSTEP*MXREPL*MXDEPL
CUNIX CALL  CLEA( DNREPL, LENG5 , -1.00E+50 )
      CALL  CLEA( DNREPL, LENG5 , -1.00E+38 )
CKK9
      CALL ICLEA( ISOREP , MXREPL , 0 )
      ITOTIO    = 0
CEND
      DO  5   M =1 , MXREPL
      NMREPL(M) =  '    '
    5 CONTINUE
C
      NFER      = 0
      NFIS      = 0
      DO   10 M = 1 , MXFISS
      NAMFIS(M) =  '    '
      NAMFER(M) =  '    '
      IFISDN(M) = 0
      FISFCT(M) = 0.0
      IFRTDN(M) = 0.0
      FRTFCT(M) = 0.0
   10 CONTINUE
      CASBRN    =  '    '
      CASINT    =  '    '
      INTSTP    = -1
C
C
       IF(NMAT.GT.MXZONE) THEN
          WRITE(NOUT1,*) ' ** PROGRAMMING FATAL ERROR STOP ST BURNIP !'
          WRITE(NOUT1,*) ' ** OVER NO OF MATERIALS :'
     1   ,': REQUESTED IS ',NMAT,' BUT RESERVED IS ',MXZONE,' !!! '
          WRITE(NOUT2,*) ' ** PROGRAMMING FATAL ERROR STOP ST BURNIP !'
          WRITE(NOUT2,*) ' ** OVER NO OF MATERIALS :'
     1   ,': REQUESTED IS ',NMAT,' BUT RESERVED IS ',MXZONE,' !!! '
          STOP 900
          ENDIF
C
      CALL  BURNEG( IBCOLP , IMAXBN , LAPSBN , II(LCNECF) , NREGBN )
C
C *** SET MATXRG
C
      NXRB     = 0
      NXR      = 0
      DO 80 NR = 1 , NRR
      IPOS     = IXR(NR)
      MPOS     = MAR(NR)
      IF(IPOS.GT.0   )  MATXRG(MPOS) = IPOS
      IF(IPOS.GT.NXRB)  NXRB         = IPOS
   80 CONTINUE
C
      DO 85 M  = 1 ,NMP
      MPOS     = MATD(M)
      IF(MPOS.GT.0) THEN
                    VOLM(MPOS) = VOLZ(M)
                    ENDIF
   85 CONTINUE
C
C *** READ BLOCK #1 INPUT DATA
C
CMOVE CALL REAM(IDUM,IBC ,IDUM,0,10,0)
C
      IBUNIT = IABS( IBC (2) )
      IBEDIT = IBC (4)
      IBC(8) = LCHA0
CKUNI
C     New OPTION was added by k.kaneko  june 20 2002 .
C
C      MODYLD  = 0 : normal option
C              = N > 0 : modify all fp yeild data replaced by N-th yeild data
C
       MODYLD  = IBC (11)
C
      IF(IBEDIT.GE.2)  WRITE(NOUT2,*) ' ** MATXRG : ',MATXRG
      IF(IBEDIT.GE.2)  WRITE(NOUT2,*) ' ** VOLM   : ',(VOLM(M),M=1,NMAT)
C
C *** READ BLOCK #2 INPUT DATA
C
      CALL REAM(DUM,DUM,POWERL,0,0,NEP)
C
      DO 70  I = 1 , NEP
      NSTP (I) =  20
      IF(I.EQ.1)             NSTP(I) = 30
      IF(POWERL(I).LE.0.0)   NSTP(I) =  1
   70 CONTINUE
C
C *** READ BLOCK #3 INPUT DATA
C
      CALL REAM(PERIOD,PERIOD,PERIOD,0,0,NEP)
C
C *** READ BLOCK #4 INPUT DATA
C
      IF(IBC(2).LT.0) THEN
                      CALL REAM( STDNUC , IDUM , IDUM , 1 , 0 , 0 )
                      IBC(2) = -IBC(2)
                      ELSE
                      STDNUC  =  'XU05'
                      ENDIF
C
C *** READ BLOCK #5 INPUT DATA
C
      IF(IABS(IBC(3)).EQ.2) THEN
                      CALL REAM( CASBRN , IDUM , IDUM , 1 , 0 , 0 )
                      IVOID  = 1
                      ELSE
                      CASBRN =  '    '
                      IVOID  = 0
                      ENDIF
C
C *** READ BLOCK #6 INPUT DATA
C
      IF(IABS(IBC(3)).EQ.4) THEN
CM                    CALL REAM( CASINT , IDUM , DUM , 1 , 2 , 0 )
CM                    INTSTP = IDUM(1)
CM                    MTREPL = IDUM(2)
                      CALL REAM( CASINT , IDUM , DUM , 1 , 1 , 0 )
                      INTSTP = IDUM(1)
                      IF(INTSTP.LT.0) THEN
                                      IKEEP = -INTSTP
                                      MTREPL = IKEEP/10000
                                      INTSTP = IKEEP - 10000*MTREPL
                                      ENDIF
                      ELSE
                      CASINT =  '    '
                      INTSTP = -1
                      ENDIF
C
C *** READ BLOCK #7 INPUT DATA
C
      IF(IBC(5).EQ.1) THEN
                      CALL REAM( DUM  , IDUM , DUM , 0 , 2 , 0 )
                      NFIS   = IDUM(1)
                      NFER   = IDUM(2)
                      DO 20  M = 1 , NFIS
                      CALL REAM( NAMFIS(M),IFISDN(M),FISFCT(M),1,1,1)
  20                  CONTINUE
                      DO 30  M = 1 , NFER
                      CALL REAM( NAMFER(M),IFRTDN(M),FRTFCT(M),1,1,1)
  30                  CONTINUE
                      ENDIF
C
C *** READ BLOCK #8 INPUT DATA
C
      IF(IBC(6).EQ.1) THEN
                      CALL REAM( DUM , NMICR , DUM , 0 , 1 , 0 )
                      CALL REAM( NAMMIC , IDUM , DUM , NMICR , 0 , 0 )
                      ELSE
                      NMICR = 0
                      ENDIF
C
C *** READ BLOCK #9 INPUT DATA
C
      IF(IBC(10).EQ.1) THEN
                      CALL REAM( DUM , IBTYPE , DUM , 0 , NMAT , 0 )
                      ENDIF
C***********************************************************************
C  START READ CHAIN LIBARY
C###########################
C
       REWIND 93
       READ(93,*) LNMAX,NTNUC,NTFISS,NPAR,NYLDTY
       READ(93,*) AWUNIT,ABOGA,EVTOJ
C
       ABOGA = ABOGA*1.00000E-24
       LCHA  = LCHA0
       IF(LNMAX.GT.MXNUC) THEN
          WRITE(NOUT1,*) ' ** PROGRAMMING FATAL ERROR STOP ST BURNIP !'
          WRITE(NOUT1,*) ' ** OVER MAXIMUM NUCLIDE NUMBER :',
     1    ' REQUESTED IS ',LNMAX,' BUT RESERVED IS ',MXNUC,' !!! '
          WRITE(NOUT2,*) ' ** PROGRAMMING FATAL ERROR STOP ST BURNIP !'
          WRITE(NOUT2,*) ' ** OVER MAXIMUM NUCLIDE NUMBER :',
     1    ' REQUESTED IS ',LNMAX,' BUT RESERVED IS ',MXNUC,' !!! '
          STOP 901
          ENDIF
C
       IF(NTFISS.GT.MXFISS) THEN
          WRITE(NOUT1,*) ' ** PROGRAMMING FATAL ERROR STOP ST BURNIP !'
          WRITE(NOUT1,*) ' ** OVER MAXIMUM FISSIONABLE NUCLIDE NUMBER :'
     1   ,': REQUESTED IS ',NTFISS,' BUT RESERVED IS ',MXFISS,' !!! '
          WRITE(NOUT2,*) ' ** PROGRAMMING FATAL ERROR STOP ST BURNIP !'
          WRITE(NOUT2,*) ' ** OVER MAXIMUM FISSIONABLE NUCLIDE NUMBER :'
     1   ,': REQUESTED IS ',NTFISS,' BUT RESERVED IS ',MXFISS,' !!! '
          STOP 902
          ENDIF
C
C
C      READ NUCLIDE INFORMATION
C
       DO 2100 I = 1 , LNMAX
       READ(93,'(A80)') LINE
       IHOL(I)   = LINE ( 1: 8)
       SRACID(I) = LINE (11:14)
       READ(LINE(19:80),*)  NCODE(I),AMASS(I),IFISS(I),IRESSB(I),
     +                      REACEV(1,I),REACEV(2,I),FACT2N(I)
       AMASS(I) = AWUNIT*AMASS(I)
C ***  CHECK REACEV : REACEV(2,I) MUST BE ZERO IN PRESENT VERSION
       IF(REACEV(1,I).GT.0.0) THEN
                              REACEV(1,I) = REACEV(1,I) + REACEV(2,I)
                              REACEV(2,I) = 0.0
                              ELSE
                              REACEV(1,I) = 0.0
                              REACEV(2,I) = 0.0
                              ENDIF
       IF(FACT2N(I).LE.0.0)  FACT2N(I) = 1.175000
 2100  CONTINUE
C
       IF(IBEDIT.GE.2) THEN
       DO 2110 I = 1 , LNMAX
       WRITE(NOUT2,2111) I,IHOL(I),NCODE(I),IFISS(I),IRESSB(I),
     +            SRACID(I),AMASS(I),(REACEV(J,I),J=1,2),FACT2N(I)
 2110  CONTINUE
                       ENDIF
C
 2111  FORMAT(1H ,' << #2 >> ',I3,2X,A8,2X,3I6,2X,A4,2X,4F10.4)
 2121  FORMAT(A8,I2,E10.3,A8)
 2122  FORMAT(A8,2X,A8,2X,E12.5)
 2123  FORMAT(1H ,' ERROR STOP AT READING BURNUP LIBARY !!! ',
     +       /1H ,' REACTION(',A8,') IS NOT FOUND FOR PARENT(',A8,
     +            ') => DAUGHTER(',A8,') CHAIN. ')
 2124  FORMAT(1H ,' ERROR STOP AT READING BURNUP LIBARY !!! ',
     +       /1H ,' TIME UNIT(',A8,') IS NOT ALLOWED FOR ',A8,' !! ')
C
C      READ #3 & #4 FOR CHAIN DATA
C
       COEFS   =  0.69314718
       COEFM   =  COEFS/60.0
       COEFH   =  COEFM/60.0
       COEFD   =  COEFH/24.0
       COEFY   =  COEFD/365.0
C
       DO 2300  LOP = 1 , NTNUC
       IF(IFISS(LOP).GE.0)  LASTFP = LASTFP + 1
C
       READ(93,2121) KDENT,NCH0,HALFT,TUNIT
       IF(IBEDIT.GE.3) WRITE(NOUT2,*) KDENT,NCH0,HALFT,TUNIT
C
       IF(NCH0.GT.NPAR) THEN
          WRITE(NOUT1,*) ' ** FATAL ERROR STOP AT BURNIP !! '
          WRITE(NOUT1,*) ' ** TOO MANY DAUGHTER PATH : ',
     1  ' REQUTESTED IS ',NCH0,' BUT NPAR OF CHAIN-LIBRARY IS ',NPAR,'!'
          WRITE(NOUT2,*) ' ** FATAL ERROR STOP AT BURNIP !! '
          WRITE(NOUT2,*) ' ** TOO MANY DAUGHTER PATH : ',
     1  ' REQUTESTED IS ',NCH0,' BUT NPAR OF CHAIN-LIBRARY IS ',NPAR,'!'
                        STOP 807
                        ENDIF
C
       IPOS        = 0
       CALL  HOLPOS(IPOS,KDENT,LNMAX,IHOL)
       NUCL (IPOS) = NCODE(IPOS)
       NCH  (IPOS) = NCH0
       RAMDA(IPOS) = 0.0
       IF(HALFT.GT.0.0)       RAMDA(IPOS) = 1.0 / HALFT
       FACT        = 0.0
       IF(TUNIT(1:1).EQ.'S')  FACT = COEFS
       IF(TUNIT(1:1).EQ.'M')  FACT = COEFM
       IF(TUNIT(1:1).EQ.'H')  FACT = COEFH
       IF(TUNIT(1:1).EQ.'D')  FACT = COEFD
       IF(TUNIT(1:1).EQ.'Y')  FACT = COEFY
       RAMDA(IPOS) = FACT*RAMDA(IPOS)
       IF(HALFT.GT.0.0.AND.FACT.EQ.0.0) THEN
                                WRITE(NOUT1,2124) TUNIT,KDENT
                                WRITE(NOUT2,2124) TUNIT,KDENT
                                STOP 809
                                ENDIF
C
       DO 2150    J   = 1 , NPAR
       NUCLP(J,IPOS) = 0
       NBIC (J,IPOS) = 0
       PBIC (J,IPOS) = 0.0
 2150  CONTINUE
C
       IF(NCH0.LE.0) GO TO 2300
C
       DO 2200  JOP = 1 , NCH0
       READ(93,2122) KDENT,IDREAC,PROB
       JPOS        = 0
       CALL  HOLPOS(JPOS,KDENT,LNMAX,IHOL)
C
       IF(PROB.LT.0.0.OR.PROB.GT.1.0) THEN
          WRITE(NOUT1,*) ' ** FATAL ERROR STOP AT BURNIP !! '
          WRITE(NOUT1,*) ' ** BRANCHING RAITO IS OVER 1.0 OR NEGATIVE !'
          WRITE(NOUT2,*) ' ** FATAL ERROR STOP AT BURNIP !! '
          WRITE(NOUT2,*) ' ** BRANCHING RAITO IS OVER 1.0 OR NEGATIVE !'
                                      STOP 809
                                      ENDIF
C
       NUCLP(JOP,IPOS) = NCODE(JPOS)
       PBIC (JOP,IPOS) = PROB
       IF(IDREAC(1:5).EQ.'BETA-') NBIC(JOP,IPOS) = 1
       IF(IDREAC(1:5).EQ.'IT   ') NBIC(JOP,IPOS) = 2
       IF(IDREAC(1:5).EQ.'CAPTU') NBIC(JOP,IPOS) = 3
       IF(IDREAC(1:5).EQ.'BETA+') NBIC(JOP,IPOS) = 4
       IF(IDREAC(1:5).EQ.'EC   ') NBIC(JOP,IPOS) = 5
       IF(IDREAC(1:5).EQ.'ALPHA') NBIC(JOP,IPOS) = 6
       IF(IDREAC(1:5).EQ.'DELAY') NBIC(JOP,IPOS) = 7
       IF(IDREAC(1:5).EQ.'2N   ') NBIC(JOP,IPOS) = 8
       IF(NBIC(JOP,IPOS).EQ.0)  THEN
                             WRITE(NOUT1,2123) IDREAC,KDENT,IHOL(IPOS)
                             WRITE(NOUT2,2123) IDREAC,KDENT,IHOL(IPOS)
                                STOP 810
                                ENDIF
 2200  CONTINUE
 2300  CONTINUE
C
       IF(IBEDIT.GE.2) THEN
       DO 2320 I = 1 , NTNUC
       WRITE(NOUT2,2301) I,IHOL(I),NCODE(I),NCH(I),RAMDA(I)
       DO 2310 J = 1 , NCH(I)
       WRITE(NOUT2,2302) J,NUCLP(J,I),NBIC(J,I),PBIC(J,I)
 2310  CONTINUE
 2320  CONTINUE
       WRITE(NOUT2,2305) LNMAX,NTNUC,NTFISS,LASTFP
                       ENDIF
C
 2301  FORMAT(1H ,' << #3 >> ',I3,2X,A8,2X,2I6,1PE12.5,6H SEC-1 )
 2302  FORMAT(1H ,' << #4 >> ',I3,2I6,F10.6)
 2304  FORMAT(5(A8,2X))
 2305  FORMAT(1H ,' ## LBNAX,NTNUC,NTFISS,LASTFP ## ',6I6)
C
C      READ #5,#6,#7 FP YIELD DATA
C
       CALL  CLEA ( GAM , NTFIS0*NTNUC0  , 0.0 )
CKUNI
       ISWYLD     = 0
       NMYLD2     = '        '
       IF(MODLYD.GT.NYLDTY) MODYLD=0
C
       DO 2400 LOP = 1 , NYLDTY
       READ(93,'(A80)') LINE
       KDENT      = LINE(1:8)
       READ(LINE(9:72),*) NYNUCL,NFP
       READ(93,2304) (IHOLF(I),I=1,NYNUCL)
       DO 2350 I   = 1 , LNMAX
       YIELDW(I)  = 0.0
 2350  CONTINUE
C
       SUM        = 0.0
       DO 2360 I   = 1 , NFP
       READ(93,'(A80)') LINE
       IHOLW(I)   = LINE(1:8)
       READ(LINE(9:72),*) YIELDW(I)
       SUM        = SUM   + YIELDW(I)
 2360  CONTINUE
C
       IF(IBEDIT.GE.4) THEN
                       WRITE(NOUT2,2391) LOP,KDENT,NYNUCL,NFP,SUM
                       WRITE(NOUT2,2392) (IHOLF(I),I=1,NYNUCL)
                       DO 2370 I   = 1 , NFP
                       WRITE(NOUT2,2393) I,IHOLW(I),YIELDW(I)
 2370                  CONTINUE
                       ENDIF
CKUNI
       NMYLD1      = KDENT
C
       DO 2390 J   = 1 , NYNUCL
       KDENT      = IHOLF(J)
       DO 2375 JJ  = 1 , NTFISS
       JPOS       = JJ
       IF(KDENT.EQ.IHOL(JJ)) GO TO 2376
 2375  CONTINUE
       WRITE(NOUT1,2394) KDENT
       WRITE(NOUT2,2394) KDENT
       GO TO 2390
C
 2376  CONTINUE
CKUNI
       if(MODYLD.eq.LOP.and.ISWYLD.eq.0) then
                        ISWYLD = JPOS
                        NMYLD2 = NMYLD1
                        endif
CEND
       DO 2380 I   = 1 , NFP
       KDENT      = IHOLW(I)
       DO 2377 III = NTFISS+1,NTNUC
       IPOS       = III
       IF(KDENT.EQ.IHOL(III)) GO TO 2378
 2377  CONTINUE
       WRITE(NOUT1,2394) KDENT
       WRITE(NOUT2,2394) KDENT
       GO TO 2380
 2378  CONTINUE
       GAM(JPOS,IPOS) = YIELDW(I)
 2380  CONTINUE
 2390  CONTINUE
 2400  CONTINUE
CKUNI
       WRITE(NOUT1,*) ' *** MODYLD ISWYLD : ',MODYLD,ISWYLD
       IF(ISWYLD.gt.0) THEN
                       IBC(11)     = ISWYLD
                       DO 2420 LOP = 1 , NTFISS
                       DO 2410 J   = 1 , NTNUC
                       GAM(LOP,J)  = GAM(ISWYLD,J)
 2410                  CONTINUE
 2420                  CONTINUE
                       ENDIF
CEND
C
       IF(IBEDIT.GE.4) THEN
                       DO 2500 LOP = 1 , NTFISS
                       WRITE(NOUT2,2395) IHOL(LOP)
                       SUM        = 0.0
                       DO 2450 J   = 1 , NTNUC
                       WRITE(NOUT2,2396) IHOL(J),GAM(LOP,J)
                       SUM        = SUM + GAM(LOP,J)
 2450                  CONTINUE
                       WRITE(NOUT2,2397) SUM
 2500                  CONTINUE
                       ENDIF
C
 2391  FORMAT(1H ,' << #5 >> ',I3,2X,A8,2X,2I6,1PE12.5)
 2392  FORMAT(1H ,' << #6 >> ',10(A8,2X),
     +       /1H ,' << #6 >> ',10(A8,2X),
     +       /1H ,' << #6 >> ',10(A8,2X))
 2393  FORMAT(1H ,' << #7 >> ',I3,2X,A8,2X,1PE12.5)
 2394  FORMAT(1H ,2X,A8,' IS NOT FOUND IN IHOL TABLE !! ')
 2395  FORMAT(1H1,' ## YIELD DATA FOR ',A8,' ## ')
 2396  FORMAT(1H ,' << KDENT YIELD  >> ',A8,2X,1PE12.5)
 2397  FORMAT(1H ,' << SUM OF YIELD >> ',1PE12.5)
C
       REWIND 93
C
C***********************************************************************
C  END READ CHAIN LIBARY
C###########################
C
C   *** SET DEFAULT CONVERSION RATIO DEFINISION
C
      IF(NFIS.EQ.0.AND.NFER.EQ.0) THEN
                     DO 2600 I = 1, NTFISS
                     IF(IFISS(I).EQ.3) THEN
                                       NFIS = NFIS + 1
                                       NAMFIS(NFIS) =SRACID(I)(2:4)//'A'
                                       IFISDN(NFIS) = 1
                                       FISFCT(NFIS) = 1.0
                                       ENDIF
                     IF(IFISS(I).EQ.2) THEN
                                       NFER = NFER + 1
                                       NAMFER(NFER) =SRACID(I)(2:4)//'C'
                                       IFRTDN(NFER) = 1
                                       FRTFCT(NFER) = 1.0
                                       ENDIF
                     IF(IFISS(I).EQ.4) THEN
                                       NFER = NFER + 1
                                       NAMFER(NFER) =SRACID(I)(2:4)//'D'
                                       IFRTDN(NFER) = 1
                                       FRTFCT(NFER) = 1.0
                                       ENDIF
 2600                CONTINUE
                     ENDIF
C
C **** COPY ORIGIANL MATERIAL INFORMATION
C
      CALL  CLEA( DN    , MXNUC*NMAT   , 0.0 )
      CALL  CLEA( DANCF , MXNUC*NMAT   , 0.0 )
      CALL ICLEA( IRES  , MXNUC*NMAT   , 0   )
      CALL ICLEA( IXMICR, MXNUC*NMAT   , 0   )
      CALL  CLEA( DC    , MXZONE       , 0.0 )
C
      L0      = NEF+NET+NERF+NERT
      L1      = L0 + 2*NMAT
      L2      = L1 + 4*NMAT
      L3      = L2 + 3*NMAT
      NTISO   = 0
C
      DO 55 I = 1,NMAT
   55 NTISO   = NTISO+II(L1+I)
C
      L4      = L3 + 2*NTISO
      L5      = L4 +   NTISO
      L6      = L5 +   NTISO
C
      IF(IBEDIT.GE.3) THEN
                      WRITE(NOUT2,161) L1,L2,L3,NTISO,L4,L5,L6
                      WRITE(NOUT2,162) (II(L1+I),I=1,NMAT)
                      WRITE(NOUT2,163) (II(L2+I),I=1,NMAT)
                      WRITE(NOUT2,164) (II(L3+I),I=1,2*NTISO)
                      WRITE(NOUT2,165) (AII(L4+I),I=1,NTISO)
                      WRITE(NOUT2,166) (II(L5+I),I=1,NTISO)
                      WRITE(NOUT2,167) (II(L6+I),I=1,NTISO)
                      ENDIF
C
  161 FORMAT(1H0,'II(1880) AT FIRST STAGE'/5X,'L1=',I4,3X,'L2=',I4,3X,
     1     'L3=',I4,3X,'NTISO=',I4,3X,'L4=',I4,3X,'L5=',I4,3X,'L6=',I4)
  162 FORMAT(1H ,4X,'NISO(I) =',25I4)
  163 FORMAT(1H ,4X,'LISO(I) =',25I4)
  164 FORMAT(1H ,4X,'IDENT(I) ='/(5X,10(2X,A4,A4,2X)))
  165 FORMAT(1H ,4X,'DN(I) ='/(5X,1P10E12.4))
  166 FORMAT(1H ,4X,'IRES(I) ='/(5X,40I3))
  167 FORMAT(1H ,4X,'IXMICR(I) ='/(5X,40I3))
C
      ISW      =  LCNISO
      DO 3010 J=  1,NMAT
      NISO(J)  =  II(ISW)
      NISOW(J) =  II(ISW)
      ISW      =  ISW + 1
 3010 CONTINUE
C
      ISW      =  LCXCDC
      DO 3020 J=  1,NMAT
      DC(J)    =  AII(ISW)
      ISW      =  ISW + 1
 3020 CONTINUE
C
      ISW      =  LCLISO
      DO 3030 J=  1,NMAT
      LISO(J)  =  II(ISW)
      ISW      =  ISW + 1
 3030 CONTINUE
C
      IST         =  LCIDNT
      DO 3050 J   =  1,NMAT
      ISW         =  IST + (LISO(J)-1)*2
      MM          =  NISO(J)
      IF(MM.LE.0)   GO TO 3050
      DO 3040  M    =  1,MM
      DO 3040   I   =  1,2
      IDENT(I,M,J)  =  CHAII(ISW)
      ISW           =  ISW+1
 3040 CONTINUE
 3050 CONTINUE
C
      IST       =  LCDN
      DO 3070 J =  1,NMAT
      ISW       =  IST + LISO(J) - 1
      MM        =  NISO(J)
      IF(MM.LE.0)   GO TO 3070
      DO 3060 M =  1,MM
      DN(M,J)   =  AII(ISW)
      ISW       =  ISW+1
 3060 CONTINUE
 3070 CONTINUE
C
      IST        =  LCIRES
      DO 3090 J  =  1,NMAT
      ISW        =  IST + LISO(J) - 1
      MM         =  NISO(J)
      IF(MM.LE.0)   GO TO 3090
      DO 3080 M  =  1,MM
      IRES(M,J)  =  II(ISW)
      ISW        =  ISW+1
 3080 CONTINUE
 3090 CONTINUE
C
      IST       =  LCIXMC
      DO 3110 J =  1,NMAT
      ISW       =  IST + LISO(J) - 1
      MM        =  NISO(J)
      IF(MM.LE.0)   GO TO 3110
      DO 3100 M =  1,MM
      IXMICR(M,J)   =  II(ISW)
      ISW           =  ISW+1
 3100 CONTINUE
 3110 CONTINUE
C
      ISW        =0
      DO 3140 N  =  1 , NMAT
      MM         =  NISO(N)
      IF(MM.LE.0) GO TO 3140
      DO 3130 M  =  1 , MM
      ISW        =  ISW + 1
      DANCF(M,N)   =  DANNEW(ISW)
 3130 CONTINUE
 3140 CONTINUE
C
C --- SET FLAG TO THE DEPLETING ZONE
C
      NTDEPZ = 0
      IPOISN = 0
      CALL  ICLEA( IIUSE , MXNUC  , 0 )
      CALL  ICLEA( NDEPZ , MXZONE , 0 )
      CALL  ICLEA( MATDPL, MXZONE , 0 )
C
      DO 210 M = 1,NMP
      I        = MATD(M)
      MMK      = NISO(I)
      IF(MMK.LE.0)  GOTO 210
C@ADD
      IF(IBC(10).EQ.1.AND.IBTYPE(I).EQ.0) GO TO 210
C
      JMIC     = 0
      DO 200 J = 1 , MMK
      DO 195 K = 1 , NTNUC
      IF(IDENT(1,J,I)(2:4).EQ.SRACID(K)(2:4)) THEN
                    IIUSE(K) = 1
                    JMIC     = 1
                    IF(IFISS(K).LE.-10)  IPOISN = 1
                    GO TO 195
                    ENDIF
  195 CONTINUE
  200 CONTINUE
C
      IF(JMIC.EQ.1) THEN
                    NDEPZ(M)  = 1
                    NTDEPZ    = NTDEPZ + 1
                    MATDPL(I) = NTDEPZ
                    ENDIF
  210 CONTINUE
C
      IF(IBEDIT.GE.2) WRITE(NOUT2,*) ' ** IPOISN NTNUC :',IPOISN,NTNUC
C
      IF(IPOISN.EQ.0) THEN
                      ISW = 0
                      DO 230 I = 1, NTNUC
                      IF(IFISS(I).LE.-10) GO TO 235
                      ISW      = I
  230                 CONTINUE
  235                 NTNUC    = ISW
C
      IF(IBEDIT.GE.2) WRITE(NOUT2,*) ' *** MODIFY NTNUC : ',NTNUC
                      ENDIF
C
      IF(NTDEPZ.GT.MXDEPL) THEN
         WRITE(NOUT1,*) ' ** ERROR STOP OVER MAX. DEPL. ZONE !!! '
         WRITE(NOUT2,*) ' ** ERROR STOP OVER MAX. DEPL. ZONE !!! '
         STOP 888
         ENDIF
C***********************************************************************
C  START READ DENSITY FOR BURNUP CAL. WITH INITIAL FUEL COMPOSITION
C  OBTAINED BY OTHER BURNUP CAL. (IBC3=4)
C###########################
C
      JTYPE   = 0
C
      IF(INTSTP.GE.0.AND.IBREST.EQ.0.AND.IVOID.EQ.0) THEN
       JTYPE  = 1
       MEMBER = CASINT  // 'BNUP'
       IF(INTSTP.GT.100) THEN
                         JTYPE  = 2
                         NOXREG = INTSTP/100
                         INTSTP = INTSTP - 100*NOXREG
                         MEMBER (5:8) = 'DN0T'
                         MEMBER (7:7) = IDTEMP(NOXREG) (4:4)
CDEL                     CALL REAM( IDUM , IDUM , IDUM , 0 , 1 , 0)
CDEL                     MTREPL = IDUM(1)
                         ENDIF
C
       ISW    = 0
       LENGTH = 0
       NSTEP0 = 0
       CALL SEARCH ( MEMBER , LENGTH , ISW )
C
C
       IF(ISW.EQ.1) THEN
         WRITE(NOUT1,*) ' ERROR STOP AT BURNIP  !!! '
         WRITE(NOUT1,*) ' BECAUSE ',MEMBER,' MEMBER IS NOT FOUND . '
         WRITE(NOUT1,*) ' SO THIS CALCULATION WILL BE CANCELLED !!! '
         WRITE(NOUT2,*) ' ERROR STOP AT BURNIP  !!! '
         WRITE(NOUT2,*) ' BECAUSE ',MEMBER,' MEMBER IS NOT FOUND . '
         WRITE(NOUT2,*) ' SO THIS CALCULATION WILL BE CANCELLED !!! '
         STOP 930
         ENDIF
C
      CALL  READ( MEMBER , IARRAY , LENGTH )
      WRITE(NOUT2,*) ' ** BURNUP RESULTS(',MEMBER,') WAS READ',
     1               ' FROM ',FILENM,' .'
      NSTEP0  =  IARRAY(1)
      NTNUCI  =  IARRAY(2)
      NZON    =  IARRAY(3)
C
      IF(NTNUCI.NE.NTNUC) THEN
         WRITE(NOUT1,*) ' ERROR STOP AT BURNIP  !!! '
         WRITE(NOUT1,*) ' BECAUSE UNMATCH NUCLIDE NO IN BURNUP-CHAIN',
     1                  ' FOR READING FUEL DENSITY CASE (IBC3=4) !! '
         WRITE(NOUT1,*) ' SO THIS CALCULATION WILL BE CANCELLED !!! '
         WRITE(NOUT2,*) ' ERROR STOP AT BURNIP  !!! '
         WRITE(NOUT2,*) ' BECAUSE UNMATCH NUCLIDE NO IN BURNUP-CHAIN',
     1                  ' FOR READING FUEL DENSITY CASE (IBC3=4) !! '
         WRITE(NOUT2,*) ' SO THIS CALCULATION WILL BE CANCELLED !!! '
         STOP 931
                          ENDIF
C
      IF((INTSTP+1).GT.NSTEP0) THEN
         WRITE(NOUT1,*) ' ERROR STOP AT BURNIP  !!! '
         WRITE(NOUT1,*) ' BECAUSE BUNRUP STEP IS SHORTER THAN REQUTESTED
     1 STEP NO',        ' FOR READING FUEL DENSITY CASE (IBC3=4) !! '
         WRITE(NOUT1,*) ' SO THIS CALCULATION WILL BE CANCELLED !!! '
         WRITE(NOUT2,*) ' ERROR STOP AT BURNIP  !!! '
         WRITE(NOUT2,*) ' BECAUSE BUNRUP STEP IS SHORTER THAN REQUTESTED
     1 STEP NO',        ' FOR READING FUEL DENSITY CASE (IBC3=4) !! '
         WRITE(NOUT2,*) ' SO THIS CALCULATION WILL BE CANCELLED !!! '
                          STOP 932
                          ENDIF
C
      IF(NZON.LT.NTDEPZ.AND.JTYPE.EQ.1) THEN
         WRITE(NOUT1,*) ' ERROR STOP AT BURNIP  !!! '
         WRITE(NOUT1,*) ' BECAUSE NO OF DELPLETING ZONES IS SMALLER THAN
     1 REQUTESTED ZONES FOR READING FUEL DENSITY CASE (IBC3=4) !! '
         WRITE(NOUT1,*) ' SO THIS CALCULATION WILL BE CANCELLED !!! '
         WRITE(NOUT2,*) ' ERROR STOP AT BURNIP  !!! '
         WRITE(NOUT2,*) ' BECAUSE NO OF DELPLETING ZONES IS SMALLER THAN
     1 REQUTESTED ZONES FOR READING FUEL DENSITY CASE (IBC3=4) !! '
         WRITE(NOUT2,*) ' SO THIS CALCULATION WILL BE CANCELLED !!! '
                          STOP 933
                          ENDIF
C
      ISW     = 30 + 3*NZON + NTNUCI + NSTEP0*( 9 + 8*NZON)
      IF(JTYPE.EQ.2) THEN
      ISW     = 14 + NTNUCI + NSTEP0*9
                     ENDIF
C
      DO 2710 M = 1 , NZON
      DO 2710 I = 1 , NTNUCI
      DO 2710 J = 1 , NSTEP0
      ISW       = ISW + 1
      DNREST(J,I,M) = RARRAY(ISW)
 2710 CONTINUE
C
      ENDIF
C
C***********************************************************************
C  START READ DENSITY FOR BRANCH-OFF CALCULATION (IBC3=+/-2)
C###########################
C
      IF(IVOID.EQ.1) THEN
       MEMBER = CASBRN  // 'BNUP'
       ISW    = 0
       LENGTH = 0
       NSTEP0 = 0
       CALL SEARCH ( MEMBER , LENGTH , ISW )
C
       IF(ISW.EQ.0) THEN
                    CALL READ ( MEMBER , NSTEP0 , 1 )
                    IF(IBEDIT.GE.2) THEN
         WRITE(NOUT2,*) ' ** NSTEP0 = ',NSTEP0, ' FROM ',FILENM
                                    ENDIF
                    ENDIF
C
       IF(ISW.EQ.1) THEN
         WRITE(NOUT1,*) ' ERROR STOP AT BURNIP  !!! '
         WRITE(NOUT1,*) ' BECAUSE ',MEMBER,' MEMBER IS NOT FOUND . '
         WRITE(NOUT1,*) ' SO THIS CALCULATION WILL BE CANCELLED !!! '
         WRITE(NOUT2,*) ' ERROR STOP AT BURNIP  !!! '
         WRITE(NOUT2,*) ' BECAUSE ',MEMBER,' MEMBER IS NOT FOUND . '
         WRITE(NOUT2,*) ' SO THIS CALCULATION WILL BE CANCELLED !!! '
         STOP 940
         ENDIF
C
      CALL  READ( MEMBER , IARRAY , LENGTH )
      NTNUCI  =  IARRAY(2)
      NZON    =  IARRAY(3)
      CALL BURNVD( RARRAY , CARRAY , DNREST , NSTEP0 , NZON   , NTNUCI )
      ENDIF
C
C **** KEEP ID NUMBER OF U-235 & XE-135 , I-135 , SM-149 , PM-149
C
      IDU235  = 0
      IDXE35  = 0
      IDI135  = 0
      IDSM49  = 0
      IDPM49  = 0
      DO 62 I = 1,NTNUC
      IF(SRACID(I)(2:4).EQ.STDNUC(2:4)) IDU235 = I
      IF(SRACID(I)(2:4).EQ.'XE5')       IDXE35 = I
      IF(SRACID(I)(2:4).EQ.'I05')       IDI135 = I
      IF(SRACID(I)(2:4).EQ.'SM9')       IDSM49 = I
      IF(SRACID(I)(2:4).EQ.'PM9')       IDPM49 = I
   62 CONTINUE
C
      IF(IDU235.EQ.0) THEN
                      DO 63 I = 1 , NTFISS
                      IF(IFISS(I).EQ.3) THEN
                                        CHAII(1) = SRACID(I)
                                        IDU235   = I
                                        GO TO 64
                                        ENDIF
   63                 CONTINUE
      WRITE(NOUT1,*) ' *** STDNUC INPUT ERROR STOP  !!! '
      WRITE(NOUT1,*) ' *** ',STDNUC,' WAS NOT FOUND IN CHAIN-LIBRARY. '
      WRITE(NOUT2,*) ' *** STDNUC INPUT ERROR STOP  !!! '
      WRITE(NOUT2,*) ' *** ',STDNUC,' WAS NOT FOUND IN CHAIN-LIBRARY. '
                      STOP 999
   64                 CONTINUE
      WRITE(NOUT1,*) ' *** STDNUC WILL BE REPLACED FROM ',STDNUC,' TO ',
     1                CHAII(1),' ** '
      WRITE(NOUT2,*) ' *** STDNUC WILL BE REPLACED FROM ',STDNUC,' TO ',
     1                CHAII(1),' ** '
                      STDNUC = CHAII(1)
                      ENDIF
C
      IF(IBEDIT.GE.2) THEN
                      WRITE(NOUT2,115) STDNUC(2:4),IDU235,
     1                                 'XE5'      ,IDXE35,
     2                                 'I05'      ,IDI135,
     3                                 'SM9'      ,IDSM49,
     4                                 'PM9'      ,IDPM49
                      WRITE(NOUT2,116) NMP,NMAT,NTDEPZ
                      WRITE(NOUT2,117) (MATD  (I),I=1,NMP)
                      WRITE(NOUT2,118) (NDEPZ (I),I=1,NMP)
                      WRITE(NOUT2,119) (MATDPL(I),I=1,NMAT)
                      ENDIF
C
  115 FORMAT(1H0,'ID NUMBER OF ',A3,' =',I4,
     1  /1H ,'ID NUMBER OF ',A3,' =',I4,9X,': ID NUMBER OF ',A3,' =',I4,
     2  /1H ,'ID NUMBER OF ',A3,' =',I4,9X,': ID NUMBER OF ',A3,' =',I4)
  116 FORMAT(1H ,'NMP = ',I3,'   :   NMAT = ',I3,'   :    NTDEPZ = ',I3)
  117 FORMAT(1H ,' >> MATD  : ',20I5)
  118 FORMAT(1H ,' >> NDEPZ : ',20I5)
  119 FORMAT(1H ,' >> MATDPL: ',20I5)
C
C***********************************************************************
C  START READ DENSITY FOR RESTART CALCULATION (IBC3<0)
C###########################
C
      IF(IBREST.EQ.1) THEN
       MEMBER = CASEID(1) // 'BNUP'
C **** MAKE 'caseBNUP' MEMBER FROM 'caseHT##'
       IF(MKBNUP.EQ.1) THEN
C
C *** MEMORY LOACTION
C
      NXR      = NXRB
      LOC00    = 1
      LOC01    = LOC00  +  NXR
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
      MXWORK   = LEMORY -  LLAST + 1
C
      IF(IBEDIT.GE.2) THEN
      WRITE(NOUT2,*) ' *** MEMORY LOACTION FOR BURNIP ROUTINE *** '
      WRITE(NOUT2,*) ' ============================================== '
      WRITE(NOUT2,7) LOC00,LOC01,LOC02,LOC03,LOC04,LOC05,
     1               LOC06,LOC07,LOC08,LOC09,LOC10,
     2   LOC11,LOC12,LOC13,LOC14,LOC15,LOC16,LOC17,LOC18,LOC19,LOC20,
     3   LOC21,LOC22,LOC23,LOC24,LOC25,LOC26,LOC27,LOC28,LOC29,LOC30,
     4   LOC31,LOC32,LOC33,LOC34,LOC35,LOC36,LOC37
      WRITE(NOUT2,*) ' ============================================== '
      WRITE(NOUT2,*) ' ** LEMORY LLAST MXWORK : ',LEMORY,LLAST,MXWORK
                      ENDIF
C
    7 FORMAT(1H ,5X,10I10)
C
      LENSMY  = 30 + 3*NTDEPZ + NTNUC  + 9*NEP1 + 8*NEP1*NTDEPZ
     1             + NEP1*NTNUC*NTDEPZ + 4*NEP1*NTDEPZ*LAPSBN   + 10000
C
      IF(MXWORK.LT.LENSMY) THEN
         WRITE(NOUT1,*) ' ** ERROR STOP AT SUBROUTINE(BUNCIP) !!! '
         WRITE(NOUT1,*) ' ** WORK DIMENSION IS INSUFFICIENT   !!! '
         WRITE(NOUT1,*) ' ** REQUESTED IS ',LLAST+LENSMY,' WORDS. '
         WRITE(NOUT1,*) ' ** BUT RESERVED ',MXWORK,' WORDS. '
         WRITE(NOUT2,*) ' ** ERROR STOP AT SUBROUTINE(BUNCIP) !!! '
         WRITE(NOUT2,*) ' ** WORK DIMENSION IS INSUFFICIENT   !!! '
         WRITE(NOUT2,*) ' ** REQUESTED IS ',LLAST+LENSMY,' WORDS. '
         WRITE(NOUT2,*) ' ** BUT RESERVED ',MXWORK,' WORDS. '
         STOP 999
         ENDIF
C
       CALL  CLEA ( RARRAY, LLAST , 0.0 )
       NTEMP  = 35
C
       KOWSTP  = 0
C------Make a member caseBNUP from caseHT## if caseBNUP not exist
       CALL BURNS0(NEP1   , IBEDIT , LAPSBN , MATDPL , NMAT   ,
     1             NXR    , VOLM   , RARRAY(LOC00)   , TITLE  , CASEID ,
     2             NTNUC  , NTDEPZ , MATXRG , IDU235 , IDXE35 , IDI135 ,
     3             IDSM49 , IDPM49 , STDNUC , NGTBN  , NOUT1  , NOUT2  ,
     4             IDTEMP , NTEMP  , MTYP   , SRACID ,
     5             RARRAY(LOC01) , RARRAY(LOC02) , RARRAY(LOC03) ,
     6             RARRAY(LOC04) , RARRAY(LOC05) , RARRAY(LOC06) ,
     7             RARRAY(LOC07) , RARRAY(LOC08) , RARRAY(LOC09) ,
     8             RARRAY(LOC10) , RARRAY(LOC11) , RARRAY(LOC12) ,
     9             RARRAY(LOC13) , RARRAY(LOC14) , RARRAY(LOC15) ,
     A             RARRAY(LOC16) , RARRAY(LOC17) , RARRAY(LOC18) ,
     B             RARRAY(LOC19) , RARRAY(LOC20) , RARRAY(LOC21) ,
     C             RARRAY(LOC22) , RARRAY(LOC23) , RARRAY(LOC24) ,
     D             RARRAY(LOC25) , RARRAY(LOC26) , RARRAY(LOC27) ,
     E             RARRAY(LOC28) , RARRAY(LOC29) , RARRAY(LOC30) ,
     F             RARRAY(LOC31) , RARRAY(LOC32) , RARRAY(LOC33) ,
     G             RARRAY(LOC34) , RARRAY(LOC35) , RARRAY(LOC36) ,
     H             RARRAY(LOC37) ,
     I             RARRAY(LLAST) , IARRAY(LLAST) , CARRAY(LLAST) ,
     J             MXWORK  , NTFISS  , LASTFP    , II(LCMTNM), KOWSTP )
C
C-----------------------------------------------------------------------
C
                       NOWSTP  =  KOWSTP
                       I79     =  NOWSTP - 1
CM          WRITE(6,*) ' ** AFTER BURNS0 : KOWSTP I79 = ',KOWSTP,I79
                       ENDIF
C
       MEMBER = CASEID(1) // 'BNUP'
       ISW    = 0
       LENGTH = 0
       CALL SEARCH ( MEMBER , LENGTH , ISW )
C
       IF(ISW.EQ.1) THEN
         WRITE(NOUT1,*) ' ERROR STOP AT BURNIP  !!! '
         WRITE(NOUT1,*) ' BECAUSE ',MEMBER,' MEMBER IS NOT FOUND . '
         WRITE(NOUT1,*) ' SO THIS CALCULATION WILL BE CANCELLED !!! '
         WRITE(NOUT2,*) ' ERROR STOP AT BURNIP  !!! '
         WRITE(NOUT2,*) ' BECAUSE ',MEMBER,' MEMBER IS NOT FOUND . '
         WRITE(NOUT2,*) ' SO THIS CALCULATION WILL BE CANCELLED !!! '
         STOP 910
         ENDIF
C
      CALL  READ( MEMBER , IARRAY , LENGTH )
      NTNUCI  =  IARRAY(2)
      NZON    =  IARRAY(3)
C-----Set Number density for Restart Calculation
      CALL BURNRS( RARRAY , CARRAY , DNREST , NOWSTP ,
     1             NZON   , NTNUCI )
C
      ENDIF
C######## END OF IF-BLOCK FOR RESTART CAL. : IF(IBREST.EQ.1)
C
C ********* REARRANGE NISO,LISO,KDENT,DN,IRES,IXMICR IN /MAINC/******
C
      ST2350      = 0.0
      INITDN      = 0
      IF(IBREST.EQ.1) THEN
                      INITDN = NOWSTP
                      ELSE
C
                      IF(IVOID.EQ.1) THEN
                                     INITDN = 1
                                     ELSE
                                     IF(INTSTP.GE.0) THEN
                                                     INITDN = INTSTP + 1
                                                     ENDIF
                                     ENDIF
                      ENDIF
C
      IF(IBEDIT.GE.3) THEN
             WRITE(NOUT2,*) ' **IBREST IVOID INTSTP INITDN NOWSTP I79 :'
     1                         ,IBREST,IVOID,INTSTP,INITDN,NOWSTP,I79
             ENDIF
C
      MTYSUM      = 0
      IFUEL       = 0
      DO 3500   M = 1 , NMAT
      IF(NISO  (M).LE.0) GO TO 3500
      IF(MATDPL(M).LE.0) GO TO 3500
      KZ          = MATDPL(M)
      ISW         = NTNUC
      CALL  ICLEA ( IBPASS , MXNUC , 0 )
      IREPL       = 0
      MPOS        = 0
      IF(INITDN.GT.0) THEN
                   IF(JTYPE.EQ.2) THEN
                                  IF(KZ.EQ.MTREPL) THEN
                                           IREPL = 1
                                           MPOS  = 1
                                           ENDIF
                                  ELSE
                                  IREPL = 1
                                  MPOS  = KZ
                                  ENDIF
                   ENDIF
C
      DO 3200  I = 1 , NTNUC
      JDENT(1,I)  = SRACID(I)
      JDENT(2,I)  =  '0001'
      JRES (I)    = IRESSB(I)
      JDN  (I)    = 0.0
      IF(IREPL.GT.0)  JDN(I) =  DNREST(INITDN,I,MPOS)
      JXMICR(I)   = 0
      JDANCF(I)   = DC(M)
      IF(IOPT(3).EQ.1) JDANCF(I) = 1.000
 3200 CONTINUE
      DO  3210  I = NTNUC+1 , MXNUC
      JDENT(1,I)  =  '    '
      JDENT(2,I)  =  '    '
      JRES (I)    = 0
      JDN  (I)    = 0.0
      JXMICR(I)   = 0
      JDANCF(I)   = 0.0
 3210 CONTINUE
C
      MMK         = NISO(M)
C
      DO 3300  J  = 1 , MMK
               DO 3250 N = 1 , NTNUC
               IF(IDENT(1,J,M)(2:4).EQ.SRACID(N)(2:4)) THEN
                     IF(INITDN.EQ.0) THEN
                      JDENT(1,N) = IDENT(1,J,M)
                      JDENT(2,N) = IDENT(2,J,M)
                      JDN    (N) =  DN    (J,M)
                      JXMICR (N) =  IXMICR(J,M)
                      JDANCF (N) =  DANCF (J,M)
                      IF(IBPASS(N).LE.0 ) THEN
                                          IBPASS(N) = J
                                          ELSE
                              IF(IBPASS(N).LT.1000)
     1                        IBPASS(N) = IBPASS(N) + 1000*J
                                          ENDIF
                      ENDIF
                      GO TO 3290
                      ENDIF
 3250          CONTINUE
                 ISW          =  ISW + 1
                 IF(ISW.GT.MXNUC) THEN
       WRITE(NOUT1,*) ' ** FATAL PROGRAMMING ERROR AT BURNIP  !! '
       WRITE(NOUT1,*) ' ** OVER MAX. NO OF NUCLIDES (MXNUC) !! ',
     1                ' ** RESEVED MXNUC IS ',MXNUC,' !! '
       WRITE(NOUT2,*) ' ** FATAL PROGRAMMING ERROR AT BURNIP  !! '
       WRITE(NOUT2,*) ' ** OVER MAX. NO OF NUCLIDES (MXNUC) !! ',
     1                ' ** RESEVED MXNUC IS ',MXNUC,' !! '
                                  STOP 903
                                  ENDIF
                 JDENT(1,ISW) =  IDENT(1,J,M)
                 JDENT(2,ISW) =  IDENT(2,J,M)
                 JDN    (ISW) =  DN    (J,M)
                 JRES   (ISW) =  IRES  (J,M)
                 JXMICR (ISW) =  IXMICR(J,M)
                 JDANCF (ISW) =  DANCF (J,M)
 3290  CONTINUE
 3300  CONTINUE
C **** CHECK DUPLICATED NUCLIDE
       DO 3350 I = 1 , ISW
       IF(IBPASS(I).GT.1000) THEN
                  ISAVE = IBPASS(I)/1000
                  JSAVE = IBPASS(I) - ISAVE*1000
         WRITE(NOUT1,*) ' ** FATAL MATERIAL COMPOSITION INPUT ERORR ! '
         WRITE(NOUT1,*) ' ** DUPLICATED NUCLIDES EXISTS IN ',M,
     1                  '-TH MATERIAL !!! '
         WRITE(NOUT1,*) ' ** ',IDENT(1,JSAVE,M),IDENT(2,JSAVE,M),
     1                  ' &  ',IDENT(1,ISAVE,M),IDENT(2,ISAVE,M),' ** '
         WRITE(NOUT2,*) ' ** FATAL MATERIAL COMPOSITION INPUT ERORR ! '
         WRITE(NOUT2,*) ' ** DUPLICATED NUCLIDES EXISTS IN ',M,
     1                  '-TH MATERIAL !!! '
         WRITE(NOUT2,*) ' ** ',IDENT(1,JSAVE,M),IDENT(2,JSAVE,M),
     1                  ' &  ',IDENT(1,ISAVE,M),IDENT(2,ISAVE,M),' ** '
                  STOP 904
                  ENDIF
 3350  CONTINUE
C
C **** SET MTYP DATA
C
      SUM1        = 0.0
      SUM2        = 0.0
      SUMHV       = 0.0
      DO 3360  N  = 1 , NTNUC
      IF(IFISS(N).GE.  1) THEN
                          SUMHV= SUMHV+ JDN(N)
                          ENDIF
      IF(IFISS(N).GE.  0) THEN
                          SUM1 = SUM1 + JDN(N)
                          ENDIF
      IF(IFISS(N).LE.-10) THEN
                          SUM2 = SUM2 + JDN(N)
                          ENDIF
 3360 CONTINUE
C
       NISO(M)     = ISW
       IF(SUMHV.GT.0.0)  THEN
                         MTYP(M ) = 1
                         ELSE
                         IF(SUM1.GT.0.0.OR.SUM2.GT.0.0) THEN
                                        MTYP(M ) = 2
                                        ENDIF
                        ENDIF
C
       IF(MTYP(M).GT.0.AND.MATXRG(M).GT.0) THEN
                                           MTYSUM = MTYSUM + MTYP(M)
                                           IFUEL  = IFUEL  + 1
                                           ENDIF
C
C **** UPDATE NISO,IDENT,DN,IXMICR,DANCF ARRAY
C
       DO 3400   J  = 1 , ISW
       IDENT(1,J,M) = JDENT(1,J)
       IDENT(2,J,M) = JDENT(2,J)
       DN     (J,M) = JDN(J)
       IRES   (J,M) = JRES(J)
       IXMICR (J,M) = JXMICR(J)
       DANCF  (J,M) = JDANCF(J)
       IF(J.EQ.IDU235.AND.MATXRG(M).GT.0) THEN
                       ST2350 = ST2350 + VOLM(M)*DN(J,M)
                       ENDIF
       IF(J.LE.NTNUC)  THEN
                       DNNEW (J,KZ ) = JDN(J)
                       ENDIF
 3400  CONTINUE
C ***  RESET IXMICR  BY IBC(6) OPTION
       IST    = 1
       IEND   = LASTFP
       IF(SUMHV.LE.0.0)   IST = NTFISS + 1
       IF(SUM2.GT.0.0.AND.SUM1.LE.0.0) THEN
                                       IST  = IEND + 1
                                       IEND = NTNUC
                                       ENDIF
       IF(SUM2.GT.0.0.AND.SUM1.GT.0.0) THEN
CDEL                                   IST  = 1
                                       IEND = NTNUC
                                       ENDIF
C
            IF(IBC(6).EQ.1) THEN
                            DO 3420   J =  IST        , IEND
                            DO 3410   L =  1          , NMICR
                            IF(NAMMIC(L)(1:3).EQ.IDENT(1,J,M)(2:4)) THEN
                                               ISAVE       = IXMICR(J,M)
                                IF(ISAVE.LE.0) ISAVE       = 1
                                IF(ISAVE.EQ.2) ISAVE       = 3
                                IF(ISAVE.GE.4) ISAVE       = 1
                                               IXMICR(J,M) = ISAVE
                                               GO TO 3415
                                               ENDIF
 3410                       CONTINUE
 3415                       CONTINUE
 3420                       CONTINUE
                            ENDIF
C
            IF(IBC(6).EQ.2) THEN
                            DO 3430   J =  IST        , IEND
                            ISAVE       = IXMICR(J,M)
             IF(ISAVE.LE.0) ISAVE       = 1
             IF(ISAVE.EQ.2) ISAVE       = 3
             IF(ISAVE.GE.4) ISAVE       = 1
                            IXMICR(J,M) = ISAVE
 3430                       CONTINUE
                            ENDIF
C
            IF(IBC(6).EQ.3) THEN
                            DO 3440   J =  IST        , IEND
                            IF(IRESSB(J).EQ.2) THEN
                                               ISAVE       = IXMICR(J,M)
                                IF(ISAVE.LE.0) ISAVE       = 1
                                IF(ISAVE.EQ.2) ISAVE       = 3
                                IF(ISAVE.GE.4) ISAVE       = 1
                                               IXMICR(J,M) = ISAVE
                                               ENDIF
 3440                       CONTINUE
                            ENDIF
C  ***  RESET IXMICR FOR PEACO ROUTINE
       IF(IABS(IOPT(5)).EQ.2) THEN
                            DO 3450   J =  IST        , IEND
                            IF(IRESSB(J).EQ.2) THEN
                                               ISAVE       = IXMICR(J,M)
                                IF(ISAVE.LE.0) ISAVE       = 1
                                IF(ISAVE.EQ.2) ISAVE       = 3
                                IF(ISAVE.GE.4) ISAVE       = 1
                                               IXMICR(J,M) = ISAVE
                                               ENDIF
 3450                       CONTINUE
                            ENDIF

C  *** SET IPBURN
       ISUM         = 0
       DO 3460    J =   IST , IEND
       ISUM         = ISUM + 1
       IPBURN(J,KZ) = ISUM
 3460  CONTINUE
C
       IF(NISO(M).LE.NTNUC) GO TO 3480
C
       DO 3470 J = NTNUC +1 , NISO(M)
       ISUM      = ISUM  + 1
       IPBURN(J,KZ) = ISUM
 3470  CONTINUE
 3480  NISOW(M)  = ISUM
 3500  CONTINUE
C
C *** CHECK WHERE FUEL MATERIAL EXISTS OR NOT
C
      IF(IFUEL.LE.0) THEN
         WRITE(NOUT1,*) ' ** WARNING : THIS PROBLEM HAS NO FUEL !! '
         WRITE(NOUT1,*) ' ** SO BURNUP CALCULATION WILL BE SKIPPED.'
         WRITE(NOUT2,*) ' ** WARNING : THIS PROBLEM HAS NO FUEL !! '
         WRITE(NOUT2,*) ' ** SO BURNUP CALCULATION WILL BE SKIPPED.'
         IOPT(20) = 0
         RETURN
         ENDIF
C
C  ***  CHECK FOR ONLY BURNABLE POISON CASE
C
       MTYPAV = MTYSUM/IFUEL
C
CKUNI
CMOD
CMOD   IF(MTYPAV.EQ.2) THEN
CMOD       IF(IABS(IBC(2)).LE.2.OR.IABS(IBC(3)).NE.3) THEN
CMOD   WRITE(NOUT1,*) ' ** WARNING : THIS BURNUP PROBREM HAS NO FUEL.'
CMOD   WRITE(NOUT1,*) ' ** FOR THIS CASE , IBC2>2 & IBC3=3 AND USER MUST
CMOD 1 BE INPUT INITIAL FLUX LEVEL (N/CM/CM/SEC) !! '
CMOD   WRITE(NOUT2,*) ' ** WARNING : THIS BURNUP PROBREM HAS NO FUEL.'
CMOD   WRITE(NOUT2,*) ' ** FOR THIS CASE , IBC2>2 & IBC3=3 AND USER MUST
CMOD 1 BE INPUT INITIAL FLUX LEVEL (N/CM/CM/SEC) !! '
C
CMOD                             STOP 970
C  ******************** READ FLUX LEVEL (N/CM/CM/SEC) ** BLOCK-10
CMOD                   ELSE
CMOD                   CALL  REAM(DUM,DUM,DUM,0,0,1)
CMOD                   FLXNRM(1) = DUM(1)
CMOD                   ENDIF
CMOD                   ENDIF
C
       IF(MTYPAV.EQ.2.AND.IVOID.EQ.0) THEN
           ICKDAY = 0
           IF(IABS(IBC(2)).EQ.3.OR.IABS(IBC(2)).EQ.4) ICKDAY = 1
C
           IF(ICKDAY.EQ.0.OR.IABS(IBC(3)).NE.3) THEN
C
       WRITE(NOUT1,*) ' ** WARNING : THIS BURNUP PROBREM HAS NO FUEL.'
       WRITE(NOUT1,*) ' ** FOR THIS CASE , |IBC2|=3 or 4 & |IBC3|=3 !!'
       WRITE(NOUT1,*) ' ** AND ',
     1      'USER MUST INPUT INITIAL FLUX LEVEL (N/CM/CM/SEC) !! '
C
       WRITE(NOUT2,*) ' ** WARNING : THIS BURNUP PROBREM HAS NO FUEL.'
       WRITE(NOUT2,*) ' ** FOR THIS CASE , |IBC2|=3 or 4 & |IBC3|=3 !!'
       WRITE(NOUT2,*) ' ** AND ',
     1      'USER MUST INPUT INITIAL FLUX LEVEL (N/CM/CM/SEC) !! '
C
                                                STOP 970
                                                ENDIF
                                      ENDIF
C  ******************* READ FLUX LEVEL (N/CM/CM/SEC) ** BLOCK-10
       IF(IABS(IBC(3)).EQ.3) THEN
                       CALL  REAM(CADUMY,IDUM,DUM,0,0,1)
                       IF(IVOID.EQ.0) FLXNRM(1) = DUM(1)
                       ENDIF
CEND
C
C *** REPLACE INPUT BURNABLE ISOTOPE'S NUMBER DENSITY IF IBC(9)>0
C
      IF(ISREPL.LE.0) GO TO 6010
C
      DO 5100 LOP = 1 , ISREPL
      CALL  REAM( NMREPL(LOP) , DUM , DUM , 1 , 0 , 0 )
 5010 CONTINUE
      MTREPL      = 0
      CALL  REAM( DUM      , MTREPL , DUM , 0 , 1 , 0 )
      IF(MTREPL.LE.0) GO TO 5100
C
      WRITE(6,*) ' ** LOP NMREPL MTREPL : ',LOP,NMREPL(LOP),MTREPL
C
      CALL REAM( DUM , DUM , DNWORK , 0 , 0 , NEP1 )
      MPOS   = MATDPL(MTREPL)
CKUNI
      NOXRG  = MATXRG(MTREPL) 
CMOD  IF(MPOS.GT.0) THEN
      IF(MPOS.GT.0.AND.NOXRG.GT.0) THEN
                    IPOS      = 0
                    DO 5030 I = 1 , NTNUC
                    IF(SRACID(I)(2:4).EQ.NMREPL(LOP)(1:3)) THEN
CKK9
                    ISOREP(LOP) = I
CEND
                    WRITE(6,*) ' * MTREPL MPOS I : ',MTREPL,MPOS,I
                    WRITE(6,*) ' * DNREPL : ',(DNWORK(KK),KK=1,NEP1)
C
                                  DO 5020 IT = 1 , NEP1
CKK9                              DNREPL(IT,I,MPOS) = DNWORK(IT)
                                  DNREPL(IT,LOP,MPOS) = DNWORK(IT)
CEND
 5020                             CONTINUE
                                  DSAVE         = DN (I,MTREPL)
CKUNI
CMOD                              DN (I,MTREPL) = DNWORK(1)
CMOD                              DNNEW(I,MPOS) = DNWORK(1)
                                  DN (I,MTREPL) = DNWORK(NOWSTP)
                                  DNNEW(I,MPOS) = DNWORK(NOWSTP)
CEND
CKUNI                    IF(I.EQ.IDU235.AND.MATXRG(MTREPL).GT.0) THEN
                         IF(I.EQ.IDU235) THEN
                                     ST2350 = ST2350   +
CKUNI1                               VOLM(MTREPL)*(DNWORK(1)-DSAVE)
     1                               VOLM(MTREPL)*(DNWORK(NOWSTP)-DSAVE)
                                     ENDIF
                                  GO TO 5040
                                  ENDIF
 5030               CONTINUE
                    ENDIF
 5040               CONTINUE
                    GO TO 5010
C
 5100 CONTINUE
C
C
C
 6010 CONTINUE
C
      ST2350  = ST2350*1.00000E+24
      IF(IBREST.EQ.0)  ST235 = ST2350
C
C *** CHECK STANDARD NUCLIDE DENSITY IF IBC(2)=5
C
       IF(ST235.LE.0.0) THEN
         IF(IBC(2).EQ.5) THEN
         WRITE(NOUT1,*) ' ERROR STOP AT BURNIP  !!! '
         WRITE(NOUT1,*) ' BECAUSE INITIAL STDNUC(',STDNUC,') DENSITY IS'
     1                 ,' ZERO !!! '
         WRITE(NOUT1,*) ' SO THIS CALCULATION WILL BE CANCELLED !!! '
         WRITE(NOUT1,*) ' PLEASE RERUN AFTER CHANGING STDNUC NAME !! '
         WRITE(NOUT2,*) ' ERROR STOP AT BURNIP  !!! '
         WRITE(NOUT2,*) ' BECAUSE INITIAL STDNUC(',STDNUC,') DENSITY IS'
     1                 ,' ZERO !!! '
         WRITE(NOUT2,*) ' SO THIS CALCULATION WILL BE CANCELLED !!! '
         WRITE(NOUT2,*) ' PLEASE RERUN AFTER CHANGING STDNUC NAME !! '
                         STOP 960
                         ELSE
                         ST235  = 1.00000
                         ENDIF
         ENDIF
C
C **** STORE NEW MATERIAL COMPOSTION DATA TO COMMON MAINC ARRAY
C
       LISO(1)   = 1
       NTISO     = NISOW(1)
       DO 3600 M = 2 , NMAT
       LISO(M)   = LISO(M-1) + NISOW(M-1)
       NTISO     = NTISO     + NISOW(M)
 3600  CONTINUE
      L4      = L3 + 2*NTISO
      L5      = L4 +   NTISO
      L6      = L5 +   NTISO
      LTOTAL  = L6 + NTISO - 1
      LCDN    = L4 + 1
      LCIRES  = L5 + 1
      LCIXMC  = L6 + 1
C
      IF(IBEDIT.GE.2) THEN
                      WRITE(NOUT2,171) NTFISS,LTOTAL,ST235
                      WRITE(NOUT2,172) NTDEPZ,(NDEPZ(I),I=1,NMP)
                      WRITE(NOUT2,173) (VOLZ(I),I=1,NMP)
                      ENDIF
C
  171 FORMAT(1H0,'CALCULATED QUANTITIES'/
     1       5X,'TOTAL NUMBER OF FISSILE NUCLEIDES (NTFISS) =',I2/
     2       5X,'TOTAL LENGTH IN II(1880) =',I3,2X,'N(U235) AT TIME 0=',
     3       E12.5)
  172 FORMAT(1H0,4X,'NUMBER OF DEPLETING ZONE =',I2/
     1       5X,'DEPLETING ZONE FLAG, NDEPZ(I) =',10I3)
  173 FORMAT(5X,'VOLUME OF EACH MATERIAL, VOLZ(I)=',(1P8E11.4))
  174 FORMAT(1H0,'TOTAL WORD LENGTH IN /MAINC/ II ARRAY IS OVER !!',
     1      /1H ,' RESEVED MEMORY IS ',I6,' WORDS BUT REQUESTED IS ',
     2       I6,' WORDS !! ')
C
      IF(LTOTAL.GT.MXINP2)  THEN
                            WRITE(NOUT2,174) MXINP2,LTOTAL
                            WRITE(NOUT2,174) MXINP2,LTOTAL
                            STOP 999
                            ENDIF
C
      ISW0      = 0
      ISW3      = L3
      ISW4      = L4
      ISW5      = L5
      ISW6      = L6
C
      DO 3800 M =  1, NMAT
      MMK       =  NISOW(M)
      II(L1+M)  =  NISOW(M)
      II(L2+M)  =  LISO(M)
      MPOS      =  MATDPL(M)
C
      IF(MMK.GT.0.AND.MPOS.LE.0) THEN
                   DO 3700    J = 1, MMK
                   ISW0         = ISW0 + 1
                   DANNEW(ISW0) = DANCF  (J,M)
                   ISW3         = ISW3 + 1
                   CHAII(ISW3)  = IDENT(1,J,M)
                   ISW3         = ISW3 + 1
                   CHAII(ISW3)  = IDENT(2,J,M)
                   ISW4         = ISW4 + 1
                   AII  (ISW4)  = DN     (J,M)
                   ISW5         = ISW5 + 1
                    II  (ISW5)  = IRES   (J,M)
                   ISW6         = ISW6 + 1
                    II  (ISW6)  = IXMICR (J,M)
 3700              CONTINUE
                   ENDIF
C
      IF(MMK.GT.0.AND.MPOS.GT.0) THEN
          IF(IBEDIT.GE.2) THEN
          WRITE(NOUT2,*) ' ** M MPOS NISO IPBURN : ',M,MPOS,NISO(M)
          WRITE(NOUT2,'(1H ,20I5)') (IPBURN(J,MPOS),J=1,NISO(M))
          ENDIF
                   DO 3750    J = 1, NISO(M)
                   IF(IPBURN(J,MPOS).LE.0) GO TO 3750
C
                   ISW0         = ISW0 + 1
                   DANNEW(ISW0) = DANCF  (J,M)
                   ISW3         = ISW3 + 1
                   CHAII(ISW3)  = IDENT(1,J,M)
                   ISW3         = ISW3 + 1
                   CHAII(ISW3)  = IDENT(2,J,M)
                   ISW4         = ISW4 + 1
                   AII  (ISW4)  = DN     (J,M)
                   ISW5         = ISW5 + 1
                    II  (ISW5)  = IRES   (J,M)
                   ISW6         = ISW6 + 1
                    II  (ISW6)  = IXMICR (J,M)
 3750              CONTINUE
                   ENDIF
 3800 CONTINUE
C
      IF(IBEDIT.GE.3) THEN
                   WRITE(NOUT2,168) L1,L2,L3,NTISO,L4,L5,L6
                   WRITE(NOUT2,162) (II(L1+I),I=1,NMAT)
                   WRITE(NOUT2,163) (II(L2+I),I=1,NMAT)
                   WRITE(NOUT2,164) (II(L3+I),I=1,2*NTISO)
                   WRITE(NOUT2,165) (AII(L4+I),I=1,NTISO)
                   WRITE(NOUT2,166) (II(L5+I),I=1,NTISO)
                   WRITE(NOUT2,167) (II(L6+I),I=1,NTISO)
C
                   WRITE(NOUT2,169)
                   DO 150 I=1,NTNUC
                   WRITE(NOUT2,170) I,SRACID(I),(DNNEW(I,J),J=1,NTDEPZ)
  150              CONTINUE
                   ENDIF
C
  168 FORMAT(1H0,'II(1880) AT LAST STAGE'/5X,'L1=',I4,3X,'L2=',I4,3X,
     1     'L3=',I4,3X,'NTISO=',I4,3X,'L4=',I4,3X,'L5=',I4,3X,'L6=',I4)
  169 FORMAT(1H0,4X,'DENSITY IN EACH ZONE'/5X,20(1H-))
  170 FORMAT(5X,I4,2X,A4,2X,1P5E12.5:/(17X,1P5E12.5))
C
C *** RESET MTNAME FOR DEPLETING MATERIALS
C
      DO 160  M = 1,NMAT
      IF(MATDPL(M).LE.0.OR.MATXRG(M).LE.0) GO TO 160
      LL        = LCMTNM + 2*M  -  1
      CHAII(LL) (2:2) = '0'
      IF(IBREST.EQ.1) THEN
                      CHAII(LL) (2:2) =  IDTEMP(NOWSTP-1) (4:4)
                      ENDIF
  160 CONTINUE
C
C-----OUTPUT NUCLIDE & CHIAN DATA TO WORK ARRAY
C     REWIND 97
C     WRITE(97) (NUCL(I),I=1,NTNUC),(NCH(I),I=1,NTNUC),
C    1          (RAMDA(I),I=1,NTNUC),
C    2          ((GAM  (J,I),J=1,NTFISS),I=1,NTNUC),
C    3          ((NUCLP(J,I),J=1,NPAR),I=1,NTNUC),
C    4          ((NBIC (J,I),J=1,NPAR),I=1,NTNUC),
C    5          ((PBIC (J,I),J=1,NPAR),I=1,NTNUC),
C    6          (FACT2N(I),I=1,NTNUC)
C     REWIND 97
C
CMOVE MEMBER = 8H@START
CMOVE CALL BURNRW( MEMBER , NUCL  , MXNUC , 1 , 1 , NTNUC , 1 , 1 ,
CMOVE              'RITE' )
C
      IF(IBEDIT.GE.4)  THEN
                   WRITE(NOUT1,*) '** CHAIN-CHECK-WRITE AT BURNIP ** '
                       DO 1977 L = 1 , NTNUC
                   WRITE(NOUT1,978) L,NUCL(L),NCH(L),RAMDA(L)
                       DO 1977 LN= 1 , NCH(L)
                   WRITE(NOUT1,979) LN,NUCLP(LN,L),NBIC(LN,L),PBIC(LN,L)
 1977                  CONTINUE
                       ENDIF
C
  978 FORMAT(1H0,' ## L  NUCL  NCH   RAMDA ## ',3I8,1PE12.5)
  979 FORMAT(1H ,' << LN NUCLP NBIC  PBIC  >> ',3I8,F10.5)
C
      MEMBER =  'NUCL    '
      CALL BURNRW( MEMBER , NUCL  , MXNUC , 1 , 1 , NTNUC , 1 , 1 ,
     1             'RITE' )
      MEMBER =  'NCH     '
      CALL BURNRW( MEMBER , NCH   , MXNUC , 1 , 1 , NTNUC , 1 , 1 ,
     1             'RITE' )
      MEMBER =  'RAMDA   '
      CALL BURNRW( MEMBER , RAMDA , MXNUC , 1 , 1 , NTNUC , 1 , 1 ,
     1             'RITE' )
      MEMBER =  'FACT2N  '
      CALL BURNRW( MEMBER , FACT2N, MXNUC , 1 , 1 , NTNUC , 1 , 1 ,
     1             'RITE' )
      MEMBER =  'GAM     '
      CALL BURNRW( MEMBER , GAM   , NTFIS0, NTNUC0,1,NTFISS,NTNUC,1,
     1             'RITE' )
      MEMBER =  'NUCLP   '
      CALL BURNRW( MEMBER , NUCLP , NPAR0 , NTNUC0,1,NPAR,NTNUC,1 ,
     1             'RITE' )
      MEMBER =  'NBIC    '
      CALL BURNRW( MEMBER , NBIC  , NPAR0 , NTNUC0,1,NPAR,NTNUC,1 ,
     1             'RITE' )
      MEMBER =  'PBIC    '
      CALL BURNRW( MEMBER , PBIC  , NPAR0 , NTNUC0,1,NPAR,NTNUC,1 ,
     1             'RITE' )
C
      IF(IVOID.EQ.1)  THEN
                MEMBER =  'DNREST  '
                CALL  BURNRW( MEMBER , DNREST , MXSTEP , MXNUC ,
     1                        NTDEPZ , NEP1   , NTNUC  , NTDEPZ,'RITE')
                      ENDIF
C
C     DEFINE LCHA PARAMETER
C
      NCHA0  = (LEMORY - NPAR*NTNUC) / ( 3*NTNUC +  2*LCHA*NTNUC)
      LOC1   = 1
      LOC2   = LOC1 + NCHA0*NTNUC
      LOC3   = LOC2 + NCHA0*NTNUC
      LOC4   = LOC3 + NCHA0*NTNUC
      LOC5   = LOC4 + NCHA0*NTNUC*LCHA
      LOC6   = LOC5 + NCHA0*NTNUC*LCHA
      LOC7   = LOC6 + NCHA0*NTNUC*LCHA
C
*     WRITE(NOUT2,*) ' ** LEMORY NPAR NTNUC  LCHA NCHA0 : ',
*    1                    LEMORY,NPAR,NTNUC,LCHA,NCHA0
C
      KTYPE  = 1
      CALL  LNCHAI ( NUCL  , RAMDA  , NCH   , NUCLP  , NBIC , PBIC  ,
     1    NOL  ,         IARRAY(LOC1) , IARRAY(LOC2) , IARRAY(LOC3) ,
     2    IARRAY(LOC4) , IARRAY(LOC5) , IARRAY(LOC6) ,
     3               NTNUC , NPAR   , NCHA0 , LCHA   , KTYPE )
C
      NCHA      = 0
      DO 4100 I = 1 , NTNUC
      IF(NOL(I).GT.NCHA) NCHA = NOL(I)
 4100 CONTINUE
C
      IF(IBEDIT.GE.2) THEN
                      WRITE(NOUT2,*) ' *** NCHA = ',NCHA
                      WRITE(NOUT2,4110) (NOL(I),I=1,NTNUC)
                      ENDIF
C
      NCHA   = NCHA + 10
C
 4110 FORMAT(1H ,' ## NOL ## ',20I5)
C
C *** PRINT OUT OF INPUT DATA FOR BURNUP
C
CKSK  WRITE(NOUT2,101)  CASEID,TITLE,
CKSK 1                 (IBC(I),I=1,10),IBREST,STDNUC(2:4)
      WRITE(NOUT2,101)  CASEID,TITLE
CKUNI WRITE(NOUT2,102) (IBC(I),I=1,10)
      WRITE(NOUT2,102) (IBC(I),I=1,11)
      IF(IBC(11).GT.0) WRITE(NOUT2,1102) NMYLD2
      WRITE(NOUT2,103) IBREST,STDNUC(2:4)
CKSK
      IF(IVOID.GT.0)  THEN
                      WRITE(NOUT2,104) CASBRN
                      ENDIF
      IF(INTSTP.GE.0) THEN
                      WRITE(NOUT2,105) CASINT,INTSTP
                      ENDIF
      WRITE(NOUT2,106) (POWERL(I),I=1,NEP)
      WRITE(NOUT2,107) (PERIOD(I),I=1,NEP)
      IF(IBC(10).EQ.1) WRITE(NOUT2,113) (IBTYPE(I),I=1,NMAT)
C *** RESET OF CALLING PERIOD FOR PEACO PIJ INTERPOLATION TABLE CAL.
      ISAVE = IBC(7)
      IBURN = 0
      IPCNT = 0
C
      IF(ISAVE.GT.0) THEN
                     KEEPIJ  = NEP + 100
                     ELSE
                     IF(ISAVE.EQ.0) THEN
                                    KEEPIJ = 1
                                    ELSE
                                    KEEPIJ = -ISAVE
                                    ENDIF
                     ENDIF
C
      WRITE(NOUT2,111) NFIS,NFER
      ILOOP = NFIS
      DO 110 I = 1 , ILOOP
      IF(I.LE.NFIS.AND.I.LE.NFER) THEN
      WRITE(NOUT2,112)  NAMFIS(I),IFISDN(I),FISFCT(I),
     1                  NAMFER(I),IFRTDN(I),FRTFCT(I)
                        ELSE
                        IF(NFIS.EQ.ILOOP) THEN
                        WRITE(NOUT2,112) NAMFIS(I),IFISDN(I),FISFCT(I)
                              ELSE
                        WRITE(NOUT2,114) NAMFER(I),IFRTDN(I),FRTFCT(I)
                              ENDIF
                        ENDIF
  110 CONTINUE
C
C@101 FORMAT(1H1,15X,' ## BURNUP INPUT DATA LISTIING ## ',
  101 FORMAT(//1H ,15X,' ## BURNUP INPUT DATA LISTING ## ',
     1    //1H ,10X,' CASE ID : ',2A4,
     2     /1H ,10X,' TITLE   : ',18A4)
  102 FORMAT(//,
     1 1H ,5X,' NO OF BURNUP STEPS .............................. ',I3,
     2/1H ,5X,' UNIT OF BURN UP ................................. ',I3,
     3/1H ,5X,' (1:2:3:4:5=MWD/T:MWD:DAYS:DELTA DAYS:% STDNUC)',
     4/1H ,5X,' BURNUP CALCULATION MODE ......................... ',I3,
     5/1H ,5X,' (1:2:3:4=NORMAL:BRACHING:CONS. FLUX:READ DEN) ',
     6/1H ,5X,' PRINTING OUTPUT OPTION .......................... ',I3,
     7/1H ,5X,' (0:1:2  =BRIEF:DETATIL:DEBUG DETAIL)          ',
     8/1H ,5X,' DEFINISION OF CONVERSION RATIO .................. ',I3,
     9/1H ,5X,' (0:1    =DEFAULT:DEFINED BY USER-INPUT)       ',
     A/1H ,5X,' OUTPUT OPTION OF EFFECITIVE MICRO. X-SECTION..... ',I3,
     B/1H ,5X,' (0:1:2:3=NONE:USER-INPUT:ALL BURNUP NUC.:ALL RESONANT)',
     C/1H ,5X,' PIJ INTERPOLATION TABLE PRODUCTION (ONLY PEACO).. ',I3,
     D/1H ,5X,' (0:1    =ALL BURNUP STEP:INITIAL BURNUP STEP) ',
     E/1H ,5X,' MAXIMUM LENGTH OF LINEAR CHAIN (DEFAULT IS 6) ... ',I3,
     F/1H ,5X,' NO OF NUCLIDE OF WHICH ND.D IS REPLACED BY INPUT. ',I3,
CKUNIG/1H ,5X,' OPTION TO SPECIFY DEPLETING MATERIAL (0:1=NO:YES) ',I3)
     G/1H ,5X,' OPTION TO SPECIFY DEPLETING MATERIAL (0:1=NO:YES) ',I3,
     H/1H ,5X,' FIXED FP YIELD DATA FOR ALL FISSION  (0:N=NO:YES) ',I3)
C
 1102 FORMAT(1H ,5X,
     I        ' FP YIELD TYPE NAME (IBC(11)>0) ...................',A8)
CEND
  103 FORMAT(
     1/1H ,5X,' RESTART OPTION (0:1=NO:YES) ..................... ',I3,
     2/1H ,5X,' STANDARD NUCLIDE NAME (STDNUC) .................. ',A3)
  104 FORMAT(
     1 1H ,5X,' BRANCHING BURNUP CASEID ......................... ',A4)
  105 FORMAT(
     1 1H ,5X,' CASEID FOR READING INITAL FUEL DENSITY .......... ',A4,
     2 1H ,5X,' BUNUP STEP NO OF READING INITAL FUEL DENSITY .... ',I3)
  106 FORMAT(/1H ,5X,
     1 ' POWER FOR EACH BURNUP STEP (MWT/CM) ............. ',1P5E12.5:/
     2 (57X,1P5E12.5:))
  107 FORMAT(/1H ,5X,
     1 ' PERIOD FOR EACH BURNUP STEP ACCORDING IBC2 ...... ',1P5E12.5:/
     2 (57X,1P5E12.5:))
C
  111 FORMAT(//1H0,20X,' << DEFINISION OF CONVERSION RATIO >> ',
     1//1H ,25X,'NUMBER OF FISSILE   =',I3
     2 /1H ,25X,'NUMBER OF FERTILE   =',I3
C                123456789012345678  123456789012
     3//1H ,6X,' FISSILE   DENSITY   MULTIPLICATION',
     4         ' FERTILE   DENSITY   MULTIPLICATION'
     5 /1H ,6X,' NAME      FLAG      FACTOR        ',
     6         ' NAME      FLAG      FACTOR        ',
     7 /1H ,6X,'-----------------------------------',
     8         '-----------------------------------')
  112 FORMAT(1H , 7X,A4,7X,I2,6X,E12.5,4X,A4,7X,I2,6X,E12.5)
  114 FORMAT(1H ,42X,A4,7X,I2,6X,E12.5)
  113 FORMAT(/1H ,5X,
     1 ' DEPLETING MATERIAL SPECIFICATION (0:1=NO:YES) ... ',20I3:/
     2 (57X,20I3:))
C
C *** END OF PROCESS
C
      RETURN
      END
