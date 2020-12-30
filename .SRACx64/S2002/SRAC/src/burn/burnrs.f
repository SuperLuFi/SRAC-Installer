      SUBROUTINE BURNRS( RARRAY , CARRAY , DNREST ,
     1                   NSTEP0 , NZON   , NTNUCI )
C
C
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
C
C
      REAL*4       RARRAY(*)
      CHARACTER*4  CARRAY(*)
      REAL*4       DNREST(MXSTEP,MXNUC,MXZONE)
C
CKSK Zero clear of density data moved 
CKSK for Branch-off & restart calculation (density for
CKSK branch calculation has alread kept in DNREST)
C ********** ZERO CLEAR ********
CKSK  LENGW  = MXNUC*MXDEPL*MXSTEP
CKSK  CALL  CLEA( DNREST, LENGW , 0.0 )
C
      IF(NTNUCI.NE.NTNUC) THEN
         WRITE(NOUT1,*) ' <<<  ERROR STOP (BURNRS)  >>>'
         WRITE(NOUT1,*) ' BECAUSE UNMATCH NUCLIDE NO IN BURNUP-CHAIN',
     1                  ' FOR RESTART BURNUP CASE (IBC3<0)'
         WRITE(NOUT1,*) ' SO THIS CALCULATION WILL BE CANCELLED !!! '
         WRITE(NOUT2,*) ' <<<  ERROR STOP (BURNRS)  >>>'
         WRITE(NOUT2,*) ' BECAUSE UNMATCH NUCLIDE NO IN BURNUP-CHAIN',
     1                  ' FOR RESTART BURNUP CASE (IBC3<0)'
         WRITE(NOUT2,*) ' SO THIS CALCULATION WILL BE CANCELLED !!! '
                          STOP 911
                          ENDIF
C
      IF(NTDEPZ.NE.NZON ) THEN
         WRITE(NOUT1,*) ' <<<  ERROR STOP (BURNRS)  >>>'
         WRITE(NOUT1,*) ' BECAUSE UNMATCH NO OF DEPLETING ZONES ',
     1                  ' FOR RESTART BURNUP CASE (IBC3<0) !! '
         WRITE(NOUT1,*) ' SO THIS CALCULATION WILL BE CANCELLED !!! '
         WRITE(NOUT2,*) ' <<<  ERROR STOP (BURNRS)  >>>'
         WRITE(NOUT2,*) ' BECAUSE UNMATCH NO OF DEPLETING ZONES ',
     1                  ' FOR RESTART BURNUP CASE (IBC3<0) !! '
         WRITE(NOUT2,*) ' SO THIS CALCULATION WILL BE CANCELLED !!! '
                          STOP 912
                         ENDIF
C
      ST235    = RARRAY(4)
      TWTHVY   = RARRAY(5)
      STDNUC   = CARRAY(12)
      ISW      = 30 + 3*NZON + NTNUCI
C
      IF(IVOID.EQ.0) GO TO  101
C
C     CASE FOR BRANCHING MODE & RESTART CASE
C     ONLY K-EFF,K-INF,INSCR,FLXNRM,POWRZN DATA WILL BE RELACED !!!
C     OTHER DATA WAS READ AT SUBROUTINE(BURNVD)  !!
C
      ISW      = ISW + 3*NSTEP0
      DO  10 I = 1 , NSTEP0-1
      ISW      = ISW + 1
      AKEFF(I) = RARRAY(ISW)
   10 CONTINUE
      ISW      = ISW + 1
      DO  20 I = 1 , NSTEP0-1
      ISW      = ISW + 1
      AKINF(I) = RARRAY(ISW)
   20 CONTINUE
      ISW      = ISW + 1
      DO  30 I = 1 , NSTEP0-1
      ISW      = ISW + 1
      INSCR(I) = RARRAY(ISW)
   30 CONTINUE
      ISW      = ISW + 1 + 2*NSTEP0
      DO  40 I = 1 , NSTEP0-1
      ISW      = ISW + 1
      FLXNRM(I)= RARRAY(ISW)
   40 CONTINUE
      ISW     = 30 + 3*NZON + NTNUCI + NSTEP0*9
      DO 60 M = 1 , NZON
      DO 50 I = 1 , NSTEP0 - 1
      POWRZN(I,M) = RARRAY(ISW)
      ISW         = ISW + 1
   50 CONTINUE
      ISW         = ISW + 1
   60 CONTINUE
      RETURN
C
C --- COPY DAYS & EXPST & U235F AND SO ON
C
  101 CONTINUE
CKSK Zero clear of number density was moved from top
      LENGW  = MXNUC*MXDEPL*MXSTEP
      CALL  CLEA( DNREST, LENGW , 0.0 )
CKSK
      DO 110 I = 1 , NSTEP0
      ISW      = ISW + 1
      DAYS(I)  = RARRAY(ISW)
  110 CONTINUE
      DO 120 I = 1 , NSTEP0
      ISW      = ISW + 1
      EXPST(I) = RARRAY(ISW)
  120 CONTINUE
      DO 140 I = 1 , NSTEP0
      ISW      = ISW + 1
      U235F(I) = RARRAY(ISW)
  140 CONTINUE
      DO 150 I = 1 , NSTEP0
      ISW      = ISW + 1
      AKEFF(I) = RARRAY(ISW)
  150 CONTINUE
      DO 160 I = 1 , NSTEP0
      ISW      = ISW + 1
      AKINF(I) = RARRAY(ISW)
  160 CONTINUE
      DO 170 I = 1 , NSTEP0
      ISW      = ISW + 1
      INSCR(I) = RARRAY(ISW)
  170 CONTINUE
      DO 180 I = 1 , NSTEP0
      ISW      = ISW + 1
      INTCR(I) = RARRAY(ISW)
  180 CONTINUE
C --- COPY POWERL & FLXNRM & POWRZN & EXPSZN & HMINV
      DO 210 I = 1 , NSTEP0
      ISW      = ISW + 1
      IF(I.LT.NSTEP0) POWERL(I) = RARRAY(ISW)
  210 CONTINUE
      DO 220 I = 1 , NSTEP0
      ISW      = ISW + 1
      FLXNRM(I)= RARRAY(ISW)
  220 CONTINUE
C
      DO 240 M = 1 , NZON
      DO 230 I = 1 , NSTEP0
      ISW      = ISW + 1
      POWRZN(I,M) = RARRAY(ISW)
  230 CONTINUE
  240 CONTINUE
C
      DO 260 M = 1 , NZON
      DO 250 I = 1 , NSTEP0
      ISW      = ISW + 1
      EXPSZN(I,M) = RARRAY(ISW)
  250 CONTINUE
  260 CONTINUE
C
      DO 280 M = 1 , NZON
      DO 270 I = 1 , NSTEP0
      ISW      = ISW + 1
      HMINV (I,M) = RARRAY(ISW)
  270 CONTINUE
  280 CONTINUE
C --- COPY DENSITY
      ISW     = 30 + 3*NZON + NTNUCI + NSTEP0*( 9 + 8*NZON)
      DO 310 M = 1 , NZON
      DO 310 I = 1 , NTNUCI
      DO 310 J = 1 , NSTEP0
      ISW       = ISW + 1
      DNREST(J,I,M) = RARRAY(ISW)
  310 CONTINUE
C **  SET CUMMWD USING EXPST & TWTHVY
      DO 320  I = 1 , NSTEP0
      CUMMWD(I) = EXPST(I)*TWTHVY
  320 CONTINUE
C **  SET DMWZON  USING EXPSZN  & HMINV
      ISKIP     = 30  +  2*NZON
      DO 330  M = 1 , NZON
      DO 330  I = 1 , NSTEP0
      DMWZON(I,M) = EXPSZN(I,M)*HMINV(1,M)*RARRAY(ISKIP+M)
  330 CONTINUE
C
C ********* END OF PROCESS
C
      RETURN
      END
