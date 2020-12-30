C***********************************************************************
      SUBROUTINE BURNIN(NMP,MATD,VOLZ,MAR,IXR,NRR)
C
      INCLUDE  'BURNPINC'
C
      CHARACTER*12  DDNAME
      CHARACTER*4   FILENM,CASEID
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
     D   ,II(1880)
C
      COMMON / WORK / A (50000)
      COMMON /PDSPDS/ DDNAME(125),IST(15),IRW(15),IOS(35),NC(5,20),
     &                IFLSW,FILENM(3),ECODE,TEMP
C
      COMMON /BURNC1/ IBC(20),NEP,NEP1,IBREST,IBCOLP,NGBN,NGTBN,
     1                IMAXBN ,LAPSBN
C
      INTEGER*4    MATD(NMP),MAR(NRR),IXR(NRR)
      REAL*4       VOLZ(NMP)
      CHARACTER*8    MEMBER
      CHARACTER*80   LINE
C
C *** INITAL SET
C
      WRITE(6,666)
      WRITE(NOUT2,667)
C
  666 FORMAT(1H0,'*** ENTER BURNIN ***')
  667 FORMAT(1H1,//40X,' BURNIN'//)
C
C *** SET NGBN & NGTBG FOR CELL CLACULATION
C
C *** IOPT(2)>0 & IOPT(12)>0  & IOPT(20)>0
C *** BURNUP CALCULATION WILL BE DONE FOR CELL MODE
C
      CALL ICLEA( IBC , 28 , 0 )
C
      NGBN    = NERF + NERT
      NGTBN   = NERT
      IBCOLP  = 0
      IBREST  = 0
      IF (IOPT(10).EQ.0) THEN
                         NGBN   = NEF + NET
                         NGTBN  = NET
                         ENDIF
C
      IMAXBN = NGBN
      LAPSBN = NGBN
C
C *** SET NGBN & NGTBG FOR EIGN-VALUE CLACULATION
C *** IOPT(2)=0 & IOPT(12)>0
C *** IF IOPT(13)>0 , IOPT(10) = 0
C *** CORE BURNUP CALCULATION MUST BE DONE USING FINE GROUP:IOPT(10)=0 !
C
      IF (IOPT(12).NE.0.AND.IOPT(2).EQ.0) THEN
      IF (IOPT(13).NE.0) THEN
                         IBCOLP  = 1
                         NGBN    = NERF + NERT
                         NGTBN   = NERT
                         IMAXBN  = NEF  + NET
                         LAPSBN  = NGBN
C ********************** NGBN & NGTBN ARE ALREADY DEFINED BY IOPT(10)
                         ELSE
                         IBCOLP = 0
                         ENDIF
                         ENDIF
C
      IFLSW     = 1
CKSK  FILENM(1) = 4HMACR
      FILENM(1) = 'MACR'
CKSK  FILENM(2) = 4HO
      FILENM(2) = 'O   '
      IF(IOPT(10).EQ.0) FILENM(2)='OWRK'
      IF(IOPT(12).NE.0.AND.IOPT(2).EQ.0) THEN
                   IF(IOPT(13).NE.0) FILENM(2)='O   '
                   ENDIF
C
      NFILE     = 0
      ECODE     = 0
      CALL FILSRC(NFILE,FILENM)
CDEL  IF(IOS(NFILE).EQ.0) THEN
CDEL                WRITE(6,*) ' *** FILE NOT OPENED DD=',DDNAME(NFILE)
CDEL                STOP
CDEL                ENDIF
C
      IF(IOS(NFILE).EQ.0) CALL OPNPDS(IRW(NFILE),IST(NFILE))
C
C *** COPY CHAIN DATA FROM FT50F001 TO FT93F001
C
       REWIND 50
       REWIND 93
C
 2101  READ(50,'(A80)',END=2201) LINE
CKSK   LINE(73:80) = 8H
       LINE(73:80) = '        '
       IF(LINE(1:1).NE.'*') WRITE(93,'(A80)') LINE
       GO TO 2101
C
 2201  REWIND 50
       REWIND 93
C
C      READ CHAIN LIBARY PARAMETER
C
       READ(93,*) LNMAX0,NTNUC0,NTFIS0,NPAR0,NYLDT0
C
       REWIND 93
C
C      READ #BLOCK-1 INPUT DATA
C
       CALL ICLEA ( IBC , 10 , 0 )
C@MOD  CALL REAM  ( IBC , IBC ,IBC,  0  , 10  , 0 )
       CALL REAM  ( IBC , IBC ,IBC,  0  , 20  , 0 )
       NEP    = IBC(1)
       NEP1   = IBC(1) + 1
       I79    = 0
C
       LCHA0  = IBC(8)
       IF(LCHA0.LE.0)       LCHA0 = 6
       IF(LCHA0.GT.NTFIS0)  LCHA0 = NTFIS0
C
      IF(NEP.GE.MXSTEP) THEN
         WRITE(NOUT1,*) ' ** ERROR STOP OVER MAX. BURNUP STEP NO ]]] '
         WRITE(NOUT2,*) ' ** ERROR STOP OVER MAX. BURNUP STEP NO ]]] '
         STOP 888
         ENDIF
C
C      READ CASEBNUP AND CHECK WHETHER THIS CASE IS RESTART CASE OR NOT
C
       MEMBER = CASEID(1) // 'BNUP'
       ISW    = 0
       LENGTH = 0
       CALL SEARCH ( MEMBER , LENGTH , ISW )
C
       NOWSTP = 1
       IF(ISW.EQ.0) THEN
         CALL READ ( MEMBER , NOWSTP , 1 )
         WRITE(6,*) ' ** NOWSTP= ',NOWSTP, ' FROM ',MEMBER,' IN ',FILENM
       ENDIF
C******IF ISW=1 & RESTART CASE , THEN SET FLAG TO MAKE XXXXBNUP MEMBER
       MKBNUP  = 0
       IF(IBC(3).LT.0.AND.ISW.EQ.1) THEN
CDEL     WRITE(NOUT1,*) ' THIS CASE WAS NOT RESTART CASE  !!! '
CDEL     WRITE(NOUT1,*) ' BECAUSE CASEBNUP MEMBER IS NOT FOUND . '
CDEL     WRITE(NOUT1,*) ' SO THIS CALCULATION WILL BE CANCELLED !!! '
CDEL     WRITE(NOUT2,*) ' THIS CASE WAS NOT RESTART CASE  !!! '
CDEL     WRITE(NOUT2,*) ' BECAUSE CASEBNUP MEMBER IS NOT FOUND . '
CDEL     WRITE(NOUT2,*) ' SO THIS CALCULATION WILL BE CANCELLED !!! '
CDEL     STOP 910
         WRITE(NOUT1,*) ' ** ',MEMBER,' FOR RESTART IS NOT FOUND . '
         WRITE(NOUT1,*) ' ** ',MEMBER,' WILL BE CREATED FROM CASEHT## ]'
         MKBNUP = 1
       ENDIF
C
       IF(IBC(3).GT.0.AND.ISW.EQ.0) THEN
         WRITE(NOUT1,*) ' THIS CASE IS CHANGED AS A RESTART CASE !!! '
         WRITE(NOUT1,*) ' BECAUSE CASEBNUP MEMBER IS FOUND . '
         WRITE(NOUT1,*) ' AND THIS CALCULATION WILL BE CONTINUED !!! '
         WRITE(NOUT2,*) ' THIS CASE IS CHANGED AS A RESTART CASE !!! '
         WRITE(NOUT2,*) ' BECAUSE CASEBNUP MEMBER IS FOUND . '
         WRITE(NOUT2,*) ' AND THIS CALCULATION WILL BE CONTINUED !!! '
         IBC(3) = -IBC(3)
       ENDIF
C
       IF(IBC(3).LT.0) THEN
         IBREST = 1
         IF(NOWSTP.GE.NEP1) THEN
         WRITE(NOUT1,*) ' WARNING ... BURNUP CACULATION WAS ALREADY',
     1                  ' FINISHED !! NOWSTP IS ',NOWSTP,' !!! '
         WRITE(NOUT1,*) ' SO THIS CALCULATION WILL BE CANCELLED !!! '
                          STOP 920
                          ENDIF
         I79   = NOWSTP - 1
       ENDIF
C
CKSK********************************************************************
C  Member caseBNUP shoud be deleted and made from caseHT## in restart
C  calculation to avoid endless burn-up calculation in multi-restart
C  calculations (When restart calculation stopped without renual of
C  caseBNUP, old caseBNUP is used for the next restart calculation)
C
       IF(IBC(3).LT.0.AND.ISW.EQ.0) THEN
         CALL DELETE (MEMBER)
CKSK     WRITE(6,*) ' ** MEMBER ',MEMBER, ' IN ',FILENM, ' IS DELETED'
         ISW = 1
         MKBNUP = 1
       ENDIF
C
C ***  SET VAIRABLE DIMENSION
C
       LOC01 = 1
C ---  NUCLP
       LOC02 = LOC01  + NPAR0 *NTNUC0
C ---  GAM
       LOC03 = LOC02  + NTFIS0*NTNUC0
C ---  NBIC
       LOC04 = LOC03  + NPAR0 *NTNUC0
C ---  PBIC
       LOC05 = LOC04  + NPAR0 *NTNUC0
C ---  IDENT
       LOC06 = LOC05  + 2*MXNUC*NMAT
C ---  IRES
       LOC07 = LOC06  +   MXNUC*NMAT
C ---  IXMICR
       LOC08 = LOC07  +   MXNUC*NMAT
C ---  DN
       LOC09 = LOC08  +   MXNUC*NMAT
C ---  DANCF
       LOC10 = LOC09  +   MXNUC*NMAT
C ---  DNREST
       LOC11 = LOC10  +   MXNUC*MXSTEP*MXZONE
C ---  MATXRG
       LOC12 = LOC11  +   NMAT
C ---  NISOW
       LOC13 = LOC12  +   NMAT
       LEMORY = MEMORY - LOC13 + 1
C
       IF(LEMORY.LT.0) THEN
          WRITE(NOUT1,*) ' **FATAL ERROR -- MEMORY OVER STOP !!! '
          WRITE(NOUT1,*) ' **REQUESTED MEMORY SIZE = ',LOC13,' WORDS.'
          WRITE(NOUT1,*) ' **RESERVED  MEMORY SIZE = ',MEMORY,' WORDS.'
          WRITE(NOUT2,*) ' **FATAL ERROR -- MEMORY OVER STOP !!! '
          WRITE(NOUT2,*) ' **REQUESTED MEMORY SIZE = ',LOC13,' WORDS.'
          WRITE(NOUT2,*) ' **RESERVED  MEMORY SIZE = ',MEMORY,' WORDS.'
          STOP 999
          ENDIF
C
       CALL CLEA ( A , LOC13  , 0.0 )
C
       CALL BURNIP(NMP   ,MATD  ,VOLZ   ,NTNUC0 ,NTFIS0 ,NPAR0 ,LEMORY,
     1             A(LOC01),A(LOC02),A(LOC03),A(LOC04),A(LOC05),
     2             A(LOC06),A(LOC07),A(LOC08),A(LOC09),A(LOC10),
     3             A(LOC13),A(LOC13),A(LOC13),II(1)   ,II(1)   ,
     4             NOWSTP  ,LCHA0   ,A(LOC11),A(LOC12),
     5             MAR     ,IXR     ,NRR     ,MKBNUP            )
C
C *** END OF PROCESS
C
      RETURN
      END
