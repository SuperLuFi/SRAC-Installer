C
C    UMCROS ROUTINE FOR SRAC USER'S MCROSS LIBRARY PRODUCTION
C
      SUBROUTINE      UMCROS(ARRAY,MEMORY,TEMP,NISO,IDENT,IRES,IOPT)
C
      CHARACTER*8     MEMBER
      REAL*8          EBOUND,UIGP,UFGP,UBGP
      CHARACTER*4     IDTMP,NFILE
C
      COMMON /MAINC / NNOPT(500)
C
      COMMON /PCOWK2/ KCOMP,KCOMPF,DELBEF,KSREG,KMAT,KNMAX,KRES,NPROB,
     1                NDOUBL,NOUT1,NOUT2,NBB,NBH,MAXN,MAXP,KDAN,
     3                KPIJ1,KPIJ2,ESCAPA,ESCAPF,GUZAI,IPLOT,MAIN(2)
      COMMON /PCOWK3/ WORK(1000)
C
      COMMON /UMCPIN/ ITAPE,MATNO,NPE,NPF,NPC,IZA,ISWUN,AM,ELOW,EHI,
     +                RFTEMP,QVALE,QVALF,QVALC,EULOW,EUHIGH
      COMMON /UMCINT/ NPMAX,IFISS,IENDUN,IDPOS,ISWTMP,MF,MT,NP,IMAX,
     +                NSYSI,NSYSO,RQTEMP,NPRT,IPRINT
C
      COMMON /UMC001/ LIBTYP,NEF,ISTART,NG,NOMESH,KFGP,NGMAX,MAXINT,NSET
      COMMON /UMC002/ ENERGY(75),EE(47),NI(46),INTNO(46),
     +                NXG(10),NFI(10),NOIG(10),MST(46),MEND(46)
      COMMON /UMC003/ EBOUND(11),UIGP(10),UFGP(10),UBGP(46)
      COMMON /UMC004/ INTBL(2000),ENGD(2000)
      COMMON /UMC005/ SIG1D(3,46)
      COMMON /UMCTMP/ NTEMP,TMPSET(40),IDTMP(40)
C
      COMMON /PDSPDS/ BUFFER(540),IFLSW,NFILE(3),ECODE,TEMPPP
C
      REAL*4          ARRAY(MEMORY)
      INTEGER*4       IWORK(1000)
      INTEGER*4       NISO(KCOMP),IRES(KNMAX,KCOMP)
      REAL*4          TEMP(KCOMP)
      CHARACTER*8     IDENT(KNMAX,KCOMP)
C
      EQUIVALENCE    (WORK(1),IWORK(1))
C
C     INITIAL SET
C
      NSYSI  = 5
      NSYSO  = NOUT1
      NPRT   = NOUT2
      IPRINT = NNOPT(19)
      IF(IPLOT.GT.0.AND.IPRINT.EQ.0) IPRINT = 1
      MATNO  = 0
      MF     = 0
      MT     = 0
      NP     = 0
      IRC    = 0
      IC8    = NNOPT(8)
      JPLOT  = 0
      IF(IC8.GE.10)  THEN
                     IC8   = IC8 - 10
                     JPLOT = 1
                     ENDIF
C
      IF(IOPT.NE.0) GO TO 51
C
      IFLSW      = 1
      NFILE(1)   = 'MCRO'
      NFILE(2)   = 'SS  '
C
      MEMBER ='CONT0000'
      CALL   READ  ( MEMBER , WORK , 14 )
      ELOW   =  WORK(11)
      EHI    =  WORK(12)
      RFTEMP =  WORK(13)
      NPMAX  = IWORK(14)
C
      IF(IPRINT.GT.0)
     @ WRITE(NPRT,11) (WORK(I),I=1,10),ELOW,EHI,RFTEMP,NPMAX
C
      IFLSW      = 1
      NFILE(1)   = 'UMCR'
      NFILE(2)   = 'OSS '
      IRC        = 0
      MEMBER     = 'CONT0001'
      CALL   SEARCH( MEMBER  ,  LENGTH , IRC )
      IF(IRC.EQ.1) GO TO 5
C
      MEMBER = 'CONT0002'
      CALL   SEARCH( MEMBER  ,  LENGTH , IRC )
      IF(IRC.EQ.1) GO TO 5
C
      MEMBER = 'CONT0003'
      CALL   SEARCH( MEMBER  ,  LENGTH , IRC )
      IF(IRC.EQ.1) GO TO 5
C
      IRC    = 0
      MEMBER = 'CONT0004'
      CALL   SEARCH( MEMBER  ,  LENGTH , IRC )
      IF(IRC.EQ.1) GO TO 5
C
      IRC    = 0
      MEMBER = 'CONT0005'
      CALL   SEARCH( MEMBER  ,  LENGTH , IRC )
      IF(IRC.EQ.1) GO TO 5
C
      CALL UMCSET(IRCODE)
      IF(IRCODE.NE.0) GO TO 1101
      RETURN
C
    5 CONTINUE
      CALL UMCENG(IC8)
      RETURN
C
   11 FORMAT(1H1,20X,'  << PUBLIC MCROSS INFORMATION LIST >> ',
     +     //1H ,10X,' TITLE FOR THIS LIBRARY  : ',10A4,
     +      /1H ,15X,' LOWER ENERGY OF DATA..... ',F10.3,' EV '
     +      /1H ,15X,' UPPER ENERGY OF DATA..... ',F10.3,' EV '
     +      /1H ,15X,' TEMPERATURE OF DATA ..... ',F10.3,' KELVIN ',
     +      /1H ,15X,' MAX. NUMBER OF DATA...... ',I10/)
   21 FORMAT(1H ,' ERROR STOP AT SBROUTINE(UMCROS). '
     + /1H ,' MEMBER(',A8,') IS NOT FOUND IN PUBLIC MCROSS LIBRARY]]'/)
C
C----SET ADDRESS OF VARIABLE DIMENSION
C
   51 CONTINUE
      NPMAX = NPMAX * 1.2
      IF(MOD(NPMAX,2).EQ.1) NPMAX = NPMAX + 1
      IF(NPMAX.LT.30000)    NPMAX = 30000
      IMAX  = NGMAX*KFGP + 100
C
      LOC1 = 1
      LOC2 = LOC1 + NPMAX
      LOC3 = LOC2 + NPMAX
      LOC4 = LOC3 + NPMAX
      LOC5 = LOC4 + NPMAX
      LOC6 = LOC5 + NPMAX
      LOC7 = LOC6 + 2*NPMAX
      LOC8 = LOC7 + IMAX
      LOC9 = LOC8 + IMAX
      LMAX = LOC9 + IMAX
      IF(LMAX.GT.MEMORY) GO TO 1201
C
CDEL  WRITE(NSYSO,'(10H  LMAX =  ,I8)') LMAX
C
C-----LOOP OF MATERIAL
C
      DO 1000 MOP = 1 , KCOMP
      MMK         = NISO(MOP)
      IF(MMK.LE.0) GO TO 1000
      RQTEMP      = TEMP(MOP)
C-----SET IDPOS
      ISWTMP      = 0
      IDPOS       = 1
      IF(NTEMP.GT.0) THEN
                     DO 120 LOP = 1 , NTEMP
                     ISW        = LOP
                     IF(RQTEMP.EQ.TMPSET(LOP)) GO TO 121
  120                CONTINUE
                     ISW        = NTEMP + 1
  121                IDPOS      = ISW
                     ENDIF
C
      IF(IDPOS.GT.NTEMP) THEN
                         NTEMP  = NTEMP + 1
                         IF(NTEMP.GT.40) GO TO 1401
                         TMPSET(NTEMP) = RQTEMP
                         ISWTMP = 1
                         ENDIF
C-----LOOP OF NUCLIDE
      DO  990 NOP = 1 , MMK
      IIRES       = IRES(NOP,MOP)
      IF(IIRES.NE.2) GO TO 990
      MEMBER      = IDENT(NOP,MOP)
      MEMBER(5:8) = '0000'
CDEL  WRITE(NSYSO,*) ' ** IDPOS NTEMP ISWTMP ** ',IDPOS,NTEMP,ISWTMP
C-----CHECK  CZZM000N MEMBER
      IFLSW       = 1
      NFILE(1)    = 'UMCR'
      NFILE(2)    = 'OSS '
      IRC         = 0
      LENGTH      = 0
      MEMBER (1:1) = 'C'
      MEMBER (5:7) = '000'
      MEMBER (8:8) = IDTMP(IDPOS) (1:1)
C
      CALL   SEARCH(  MEMBER  ,  LENGTH , IRC )
      IF(IRC.EQ.0) THEN
                   WRITE(NSYSO,806) MEMBER
                   GO TO 990
                   ENDIF
C
      IFLSW       = 1
      NFILE(1)    = 'MCRO'
      NFILE(2)    = 'SS  '
      IRC         = 0
      LENGTH      = 0
      MEMBER(1:1) = 'C'
      MEMBER(5:8) = '0000'
      CALL  SEARCH ( MEMBER , LENGTH ,IRC )
C
      IF(IRC.EQ.1) THEN
                   WRITE(NOUT1,21) MEMBER
                   WRITE(NOUT2,21) MEMBER
                   STOP 992
                   ENDIF
C
      CALL READ ( MEMBER , IWORK , LENGTH )
      ITAPE   = IWORK(01)
      MATNO   = IWORK(02)
      NPE     = IWORK(03)
      NPF     = IWORK(04)
      NPC     = IWORK(05)
      IZA     = IWORK(06)
      ISWUN   = IWORK(07)
      AM      =  WORK(08)
      ELOW    =  WORK(09)
      EHI     =  WORK(10)
      RFTEMP  =  WORK(11)
      QVALE   =  WORK(12)
      QVALF   =  WORK(13)
      QVALC   =  WORK(14)
      EULOW   =  WORK(15)
      EUHIGH  =  WORK(16)
C
*     WRITE(NSYSO,801) MEMBER
*     WRITE(NSYSO,802) (IWORK(I),I=1,7)
*     WRITE(NSYSO,804) ( WORK(I),I=8,16)
*     WRITE(NSYSO,805) (IWORK(I),I=17,18)
*     WRITE(NSYSO,807) (EBOUND(I),I=1,NOMESH+1)
C
* 801 FORMAT(//1H ,10X,' << MEMBER(',A8,') OF PUBLIC MCROSS >> '//)
* 802 FORMAT(1H ,' ## WORK ## ',10I11)
* 804 FORMAT(1H ,' ## WORK ## ',1P10E11.4)
* 805 FORMAT(1H ,' ## WORK ## ',10A4)
  806 FORMAT(1H ,8X,' NUCLIDE(',A8,') ALREADY EXISTS IN USER MCROSS.')
* 807 FORMAT(1H ,' ##EBOUND## ',1P10E11.4)
C
      IFISS   = 1
      IF(NPF.EQ.0) IFISS= 0
      IENDUN  = 0
C-----ZERO CLEAR OF SIG1D ARRAY
      CALL  CLEA( SIG1D , 46*3 , 0.0 )
C-----ELASTIC
      IF(NPE.GT.0) THEN
                   IFLSW       = 1
                   NFILE(1)    = 'MCRO'
                   NFILE(2)    = 'SS  '
                   MEMBER(1:1) = 'F'
                   MEMBER(5:8) = 'E000'
                   LENG        = NPE*2
                   CALL READ ( MEMBER , ARRAY(LOC3) ,LENG )
                   CALL UMCRED(ARRAY(LOC1),ARRAY(LOC2),ARRAY(LOC3),
     +                         NPMAX      ,NPE)
C
                   IFLSW       = 1
                   NFILE(1)    = 'UMCR'
                   NFILE(2)    = 'OSS '
                   MF          = 3
                   MT          = 2
                   NP          = NPE
                   CALL UMCCRS(ARRAY(LOC1),ARRAY(LOC2),QVALE,MEMBER,
     +                         ARRAY(LOC3),ARRAY(LOC4),ARRAY(LOC5) ,
     +                         ARRAY(LOC6),ARRAY(LOC7),ARRAY(LOC8) ,
     +                         ARRAY(LOC9),JPLOT  )
                   ENDIF
C-----FISSION
      IF(NPF.GT.0) THEN
                   IFLSW       = 1
                   NFILE(1)    = 'MCRO'
                   NFILE(2)    = 'SS  '
                   MEMBER(1:1) = 'F'
                   MEMBER(5:8) = 'F000'
                   LENG        = NPF*2
                   CALL READ  ( MEMBER , ARRAY(LOC3) , LENG )
                   CALL UMCRED(ARRAY(LOC1),ARRAY(LOC2),ARRAY(LOC3),
     +                         NPMAX      ,NPF)
                   IFLSW       = 1
                   NFILE(1)    = 'UMCR'
                   NFILE(2)    = 'OSS '
                   MF          = 3
                   MT          = 18
                   NP          = NPF
                   CALL UMCCRS(ARRAY(LOC1),ARRAY(LOC2),QVALF,MEMBER,
     +                         ARRAY(LOC3),ARRAY(LOC4),ARRAY(LOC5) ,
     +                         ARRAY(LOC6),ARRAY(LOC7),ARRAY(LOC8) ,
     +                         ARRAY(LOC9),JPLOT  )
                   ENDIF
C-----CAPTURE
      IF(NPC.GT.0) THEN
                   IFLSW       = 1
                   NFILE(1)    = 'MCRO'
                   NFILE(2)    = 'SS  '
                   MEMBER(1:1) = 'F'
                   MEMBER(5:8) = 'C000'
                   LENG        =  NPC*2
                   CALL READ  ( MEMBER , ARRAY(LOC3) ,LENG )
                   CALL UMCRED(ARRAY(LOC1),ARRAY(LOC2),ARRAY(LOC3),
     +                         NPMAX      ,NPC)
                   IFLSW       = 1
                   NFILE(1)    = 'UMCR'
                   NFILE(2)    = 'OSS '
                   MF          = 3
                   MT          = 102
                   NP          = NPC
                   CALL UMCCRS(ARRAY(LOC1),ARRAY(LOC2),QVALC,MEMBER,
     +                         ARRAY(LOC3),ARRAY(LOC4),ARRAY(LOC5) ,
     +                         ARRAY(LOC6),ARRAY(LOC7),ARRAY(LOC8) ,
     +                         ARRAY(LOC9),JPLOT  )
                   ENDIF
C
      IF(ISWTMP.EQ.0) GO TO 990
C
C     OUTPUT 'CONTTEMP' TO USER'S MCROSS LIBRARY
C
      LENG     =  81
      MEMBER   = 'CONTTEMP'
      IRC      = 0
      CALL SEARCH ( MEMBER , LEN  , IRC )
      IF(IRC.EQ.0) CALL DELETE ( MEMBER )
      CALL WRITE  ( MEMBER , NTEMP , LENG )
      ISWTMP   = 0
  990 CONTINUE
 1000 CONTINUE
C
C     END OF PROCESS
C
      RETURN
C
 1101 CONTINUE
      WRITE(NOUT1,1202)
      WRITE(NOUT2,1202)
      STOP 991
C
 1102 FORMAT(//1H ,' ERROR STOP AT SUBROUTINE(UMCROS) ]]] ',
     +        /1H ,' FAST ENERGY STRUCTURE OF THIS PROBLEM IS DIFFERENT'
     +     ,' FROM THE ENERGY STRUCURE DEFINED IN UMCROSS LIBRARY.',
     +        /1H ,' PLEASE RERUN AND MAKE NEW UMCROSS DATA SET ]]'/)
C
 1201 CONTINUE
      WRITE(NOUT1,1202) LMAX,MEMORY
      WRITE(NOUT2,1202) LMAX,MEMORY
      STOP 993
C
 1202 FORMAT(/1H ,10X,'DIMENSION OVER ----- STOP EXECUTED AT UMCROS ]]',
     +       /1H ,10X,'REQUIRED MEMORY ---- ',I10,
     +       /1H ,10X,'RESERVED MEMORY ---- ',I10)
C
 1401 CONTINUE
      WRITE(NOUT1,1402)
      WRITE(NOUT2,1402)
      STOP 994
C
 1402 FORMAT(//1H ,' ERROR STOP AT SUBROUTINE(UMCROS) ]]] ',
     +        /1H ,' TEMPERATURE TABLE IS OVERFLOWED. (MAX=40)  '
     +        /1H ,' PLEASE RERUN AND MAKE NEW UMCROSS DATA SET.'/)
      END
