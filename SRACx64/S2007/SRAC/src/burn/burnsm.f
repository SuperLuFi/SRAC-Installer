       SUBROUTINE  BURNSM ( NOWSTP , IBEDIT , LAPSE  , MATDPL , NMAT   ,
     1                      NXR    , VOLM   , VOLX   , TITLE  , CASEID ,
     2                      NTNUC  , NTDEPZ , MATXRG , IDU235 ,
     3                      IDXE35 , IDI135 , IDSM49 , IDPM49 , STDNUC ,
     4                      LAPSET , NOUT1  , NOUT2  , IDTEMP , NTEMP  ,
     5                      MTYP   , SRACID ,          VLDEPX , GAMAV  ,
     6                      YDXE   , YDIO   , YDSM   , YDPM   , DNSITY ,
     7                      SIGXE  , SIGIO  , SIGSM  , SIGPM  , MTYPX  ,
     8                      POWRX  , EXPSX  , U235FX , HMINVX , GAMAVX ,
     9                      YDXEX  , YDIOX  , YDSMX  , YDPMX  , DENSX  ,
     A                      SIGXEX , SIGIOX , SIGSMX , SIGPMX , AFISSX ,
     B                      CFERTX , FLUXX  , AFISSM , CFERTM , FLUXM  ,
     C                      SIGA   , SIGF   , ABSRAT , ABSMAT , ABSXRG ,
     D                      ARRAY  , IARRAY , CARRAY ,
     E                      LENWRK , NTFISS , LASTFP , MTNAME )
C
      INCLUDE  'BURNPINC'
C
       REAL*4         INSCR,INTCR
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
     B                DMWZON(MXSTEP,MXDEPL)
C
C **** LOCAL ARRAY
C
       REAL*4       TWTHVY(MXSTEP)
C
C **** ARRAY DECLEARATION FOR ARGUMENT ARRAY
C
       INTEGER*4    MTYP(NMAT),MATXRG(NMAT),MATDPL(NMAT),MTYPX(NXR)
       REAL*4       VOLM(NMAT),VOLX(NXR),VLDEPX(NXR)
C
       CHARACTER*4  STDNUC,IDTEMP(NTEMP),TITLE(18)
       CHARACTER*8  CASEID,MEMBER
C
       REAL*4        ARRAY(LENWRK)
       INTEGER*4    IARRAY(LENWRK)
       CHARACTER*4  CARRAY(LENWRK),SRACID(NTNUC)
       CHARACTER*8  MTNAME(NMAT)
C
       REAL*4       GAMAV (NOWSTP,NTDEPZ)
       REAL*4       YDXE  (NOWSTP,NTDEPZ),YDIO  (NOWSTP,NTDEPZ)
       REAL*4       YDSM  (NOWSTP,NTDEPZ),YDPM  (NOWSTP,NTDEPZ)
       REAL*4       DNSITY(NOWSTP,NTNUC,NTDEPZ)
       REAL*4       SIGXE (LAPSE,NOWSTP,NTDEPZ)
       REAL*4       SIGIO (LAPSE,NOWSTP,NTDEPZ)
       REAL*4       SIGSM (LAPSE,NOWSTP,NTDEPZ)
       REAL*4       SIGPM (LAPSE,NOWSTP,NTDEPZ)
       REAL*4       FLUXM (LAPSE,NOWSTP,NMAT)
       REAL*4       FLUXX (LAPSE,NOWSTP,NXR )
       REAL*4       SIGA  (LAPSE,NOWSTP,NMAT)
       REAL*4       SIGF  (LAPSE,NOWSTP,NMAT)
       REAL*4       ABSRAT(NOWSTP,NMAT)
       REAL*4       ABSMAT(NOWSTP,NTNUC,NTDEPZ)
       REAL*4       ABSXRG(NOWSTP,NTNUC,NXR )
C
       REAL*4       POWRX (NOWSTP,NXR) , EXPSX (NOWSTP,NXR)
       REAL*4       U235FX(NOWSTP,NXR) , HMINVX(NOWSTP,NXR)
       REAL*4       GAMAVX(NOWSTP,NXR)
       REAL*4       YDXEX (NOWSTP,NXR) , YDIOX (NOWSTP,NXR)
       REAL*4       YDSMX (NOWSTP,NXR) , YDPMX (NOWSTP,NXR)
       REAL*4       DENSX (NOWSTP,NTNUC,NXR)
       REAL*4       SIGXEX(LAPSE,NOWSTP,NXR)
       REAL*4       SIGIOX(LAPSE,NOWSTP,NXR)
       REAL*4       SIGSMX(LAPSE,NOWSTP,NXR)
       REAL*4       SIGPMX(LAPSE,NOWSTP,NXR)
       REAL*4       AFISSX(LAPSE,NOWSTP,NXR)
       REAL*4       CFERTX(LAPSE,NOWSTP,NXR)
       REAL*4       AFISSM(LAPSE,NOWSTP,NTDEPZ)
       REAL*4       CFERTM(LAPSE,NOWSTP,NTDEPZ)
C
C **** WRITE BURNUP HISTORY DATA INT0 MACROWORK/MACRO FILE
C      MEMBER NAME  IS 'CASEHTNN'  NN IS NOWSTP
C
       ITLAST     = 0
       CALL   CLEA  ( TWTHVY , MXSTEP , 0.0 )
C
       DO 1000 IT = 1 , NOWSTP - 1
       MEMBER     = CASEID(1:4)// 'HT00'
       WRITE(MEMBER(7:8),'(I2.2)') IT
       JSW        = 0
       CALL SEARCH (  MEMBER , LENG  , JSW  )
       IF(JSW.NE.0) GO TO 1010
       CALL READ   ( MEMBER , ARRAY , LENG )
       ITLAST     =  IT
       CALL   ICLEA ( MTYPX  , NXR , 0 )
C
       AKEFF (IT) =  ARRAY( 5)
       AKINF (IT) =  ARRAY( 6)
       FLXNRM(IT) =  ARRAY( 9)
       FACNRM(IT) =  ARRAY(10)
       FISABS(IT) =  ARRAY(11)
       FRTCAP(IT) =  ARRAY(12)
       FDECAY(IT) =  ARRAY(13)
       CDECAY(IT) =  ARRAY(14)
       INSCR (IT) =  ARRAY(15)
        ISW       = 20
       IF(IT.LE.1)    ST235   =  ARRAY(18)
C
        DO  110 M = 1 , NMAT
        MPOS      = MATDPL(M)
        IF(MPOS.GT.0) THEN
                      ISW   =  ISW + 1
                      POWRZN(IT,MPOS) = ARRAY(ISW)
                      ENDIF
  110   CONTINUE
C
        DO  125 M = 1 , NMAT
        MPOS      = MATDPL(M)
        IF(MPOS.GT.0) THEN
                      ISW   =  ISW + 1
                      DMWZON(IT+1,MPOS) = ARRAY(ISW)
                      ENDIF
  125   CONTINUE
C
        DO  120 M = 1 , NMAT
        MPOS      = MATDPL(M)
        IF(MPOS.GT.0) THEN
                      ISW   =  ISW + 1
                      EXPSZN(IT+1,MPOS) = ARRAY(ISW)
                      ENDIF
  120   CONTINUE
C
        IF(IT.EQ.1) THEN
                    DO  130 M = 1 , NMAT
                    MPOS      = MATDPL(M)
                    IF(MPOS.GT.0) THEN
                                  ISW   =  ISW + 1
                                  HMINV(IT,MPOS) = ARRAY(ISW)
                                  ENDIF
  130               CONTINUE
                    ENDIF
C
        DO  140 M = 1 , NMAT
        MPOS      = MATDPL(M)
        IF(MPOS.GT.0) THEN
                      ISW   =  ISW + 1
                      HMINV(IT+1,MPOS) = ARRAY(ISW)
                      ENDIF
  140   CONTINUE
C
        DO  150 M = 1 , NMAT
        MPOS      = MATDPL(M)
        IF(MPOS.GT.0) THEN
                      ISW   =  ISW + 1
                      GAMAV(IT,MPOS) = ARRAY(ISW)
C
CDEL    WRITE(6,*) ' ** IT MPOS ISW GAMAV : ',IT,MPOS,ISW,(ARRAY(I),
CDEL 1                                        I=ISW,ISW+5)
C
                      ENDIF
  150   CONTINUE
C
        DO  160 M = 1 , NMAT
        MPOS      = MATDPL(M)
        IF(MPOS.GT.0) THEN
                      ISW   =  ISW + 1
                      YDXE(IT,MPOS) = ARRAY(ISW)
                      ENDIF
  160   CONTINUE
C
        DO  170 M = 1 , NMAT
        MPOS      = MATDPL(M)
        IF(MPOS.GT.0) THEN
                      ISW   =  ISW + 1
                      YDIO(IT,MPOS) = ARRAY(ISW)
                      ENDIF
  170   CONTINUE
C
        DO  180 M = 1 , NMAT
        MPOS      = MATDPL(M)
        IF(MPOS.GT.0) THEN
                      ISW   =  ISW + 1
                      YDSM(IT,MPOS) = ARRAY(ISW)
                      ENDIF
  180   CONTINUE
C
        DO  190 M = 1 , NMAT
        MPOS      = MATDPL(M)
        IF(MPOS.GT.0) THEN
                      ISW   =  ISW + 1
                      YDPM(IT,MPOS) = ARRAY(ISW)
                      ENDIF
  190   CONTINUE
C
        IF(IT.EQ.1) THEN
                    DO  200 M = 1 , NTDEPZ
                    DO  200 I = 1 , NTNUC
                    ISW       =  ISW + 1
                    DNSITY(1,I,M) = ARRAY(ISW)
  200               CONTINUE
                    ENDIF
C
        DO  210 M = 1 , NTDEPZ
        DO  210 I = 1 , NTNUC
        ISW       =  ISW + 1
        DNSITY(IT+1,I,M) = ARRAY(ISW)
  210   CONTINUE
C
        DO  300 M = 1 , NMAT
        MPOS      = MATDPL(M)
        IF(MPOS.LE.0) GO TO  300
        DO  250 N = 1 , LAPSE
        ISW       = ISW +1
        SIGXE(N,IT,MPOS) = ARRAY(ISW)
  250   CONTINUE
  300   CONTINUE
C
        DO  400 M = 1 , NMAT
        MPOS      = MATDPL(M)
        IF(MPOS.LE.0) GO TO  400
        DO  350 N = 1 , LAPSE
        ISW       = ISW +1
        SIGIO(N,IT,MPOS) = ARRAY(ISW)
  350   CONTINUE
  400   CONTINUE
C
        DO  500 M = 1 , NMAT
        MPOS      = MATDPL(M)
        IF(MPOS.LE.0) GO TO  500
        DO  450 N = 1 , LAPSE
        ISW       = ISW +1
        SIGSM(N,IT,MPOS) = ARRAY(ISW)
  450   CONTINUE
  500   CONTINUE
C
        DO  600 M = 1 , NMAT
        MPOS      = MATDPL(M)
        IF(MPOS.LE.0) GO TO  600
        DO  550 N = 1 , LAPSE
        ISW       = ISW +1
        SIGPM(N,IT,MPOS) = ARRAY(ISW)
  550   CONTINUE
  600   CONTINUE
C
        DO  700 M = 1 , NMAT
        MPOS      = MATDPL(M)
        IF(MPOS.LE.0) GO TO  700
        DO  650 N = 1 , LAPSE
        ISW       = ISW +1
        AFISSM(N,IT,MPOS) = ARRAY(ISW)
CM      WRITE(6,*) ' ** IT MPOS NG AFISSM : ',IT,MPOS,N,ARRAY(ISW)
  650   CONTINUE
  700   CONTINUE
C
        DO  800 M = 1 , NMAT
        MPOS      = MATDPL(M)
        IF(MPOS.LE.0) GO TO  800
        DO  750 N = 1 , LAPSE
        ISW       = ISW +1
        CFERTM(N,IT,MPOS) = ARRAY(ISW)
CM      WRITE(6,*) ' ** IT MPOS NG CFERTM : ',IT,MPOS,N,ARRAY(ISW)
  750   CONTINUE
  800   CONTINUE
C
        DO  900 M = 1 , NMAT
CDEL    MPOS      = MATXRG(M)
CDEL    IF(MPOS.LE.0) GO TO  900
        ISW       = ISW +1
        ABSRAT(IT,M) = ARRAY(ISW)
  900   CONTINUE
C
        DO  950 M = 1 , NMAT
CDEL    IF(MATXRG(M).LE.0) GO TO  950
        DO  910 N = 1 , LAPSE
        ISW       = ISW +1
        FLUXM(N,IT,M) = ARRAY(ISW)
  910   CONTINUE
        DO  920 N = 1 , LAPSE
        ISW       = ISW +1
        SIGF (N,IT,M) = ARRAY(ISW)
  920   CONTINUE
        DO  930 N = 1 , LAPSE
        ISW       = ISW +1
        SIGA (N,IT,M) = ARRAY(ISW)
  930   CONTINUE
  950   CONTINUE
C
        DO  960 M = 1 , NTDEPZ
        DO  960 I = 1 , NTNUC
        ISW       = ISW +1
        ABSMAT(IT,I,M) = ARRAY(ISW)
  960   CONTINUE
C
        IF(IBEDIT.GT.0) THEN
         WRITE(NOUT1,*) ' MEMBER(',MEMBER,') OF LENGTH ',ISW,' WORDS',
     1       ' WAS READ TEMPORARY IN MACROWORK/MCRO FILE '
              ENDIF
 1000   CONTINUE
C
C ***   EDIT OF X-REGION  ***
C
 1010   KOWSTP = ITLAST +1
        DO 1015 M = 1 , NMAT
        MPOS      = MATDPL(M)
        ICHK      = MATXRG(M)
        IF(MPOS*ICHK.GT.0) THEN
                           DO 1014 IT = 1 , KOWSTP
                TWTHVY(IT) = TWTHVY(IT) + HMINV(IT,MPOS)*VOLM(M)
 1014                      CONTINUE
                           ENDIF
 1015   CONTINUE
C
C ***   EDIT INTEGRATED CONVERSION RATIO
C
        CALL CLEA  ( INTCR  , MXSTEP          , 0.0 )
        SUMABS     = 0.0
        SUMCAP     = 0.0
        DO 1020 IT = 1 , ITLAST
        IT1        = IT       +  1
        DELT       = DAYS(IT1) - DAYS(IT)
CKSK    SUMABS     = SUMABS + DELT*FACNRM(IT)*(FISABS(IT)+FDECAY(IT))
CKSK    SUMCAP     = SUMCAP + DELT*FACNRM(IT)*(FRTCAP(IT)+CDECAY(IT))
        SUMABS     = SUMABS + DELT*(FACNRM(IT)*FISABS(IT)+FDECAY(IT))
        SUMCAP     = SUMCAP + DELT*(FACNRM(IT)*FRTCAP(IT)+CDECAY(IT))
        IF(SUMABS.GT.0.0)  INTCR(IT) = SUMCAP / SUMABS
C
CDEL    WRITE(6,*) ' ** IT DELT FACNRM FISABS FDECAY FRTCAP CDECAY ** '
CDEL    WRITE(6,*) IT,DELT,FACNRM(IT),FISABS(IT),FDECAY(IT),
CDEL 1             FRTCAP(IT),CDECAY(IT),SUMABS,SUMCAP
C
 1020   CONTINUE
C
C ***   EDIT X-REGION POWER & HEAVY METAL WEIGTH
C
        CALL  CLEA ( POWRX  ,  NOWSTP*NXR     , 0.0 )
        CALL  CLEA ( HMINVX ,  NOWSTP*NXR     , 0.0 )
C
        DO 1100  M = 1 , NMAT
        MPOS       = MATXRG(M)
        NPOS       = MATDPL(M)
        IF(MPOS.GT.0.AND.NPOS.GT.0) THEN
              FACT = VOLM(M)/VOLX(MPOS)
              DO 1050 IT = 1 , ITLAST
              POWRX(IT,MPOS)  = POWRX (IT,MPOS) + POWRZN(IT,NPOS)*FACT
              HMINVX(IT,MPOS) = HMINVX(IT,MPOS) + HMINV(IT,NPOS) *FACT
 1050         CONTINUE
              JT         = ITLAST + 1
              HMINVX(JT,MPOS) = HMINVX(JT,MPOS) + HMINV(JT,NPOS) *FACT
              ENDIF
 1100   CONTINUE
C
C ***   EDIT X-REGION EXPOSURE
C
        CALL  CLEA ( EXPSX ,  NOWSTP*NXR , 0.0 )
        CALL  CLEA ( ARRAY ,  NXR        , 0.0 )
        DO 1110  M = 1 , NMAT
        MPOS       = MATXRG(M)
        NPOS       = MATDPL(M)
        IF(MPOS.GT.0.AND.NPOS.GT.0) THEN
                ARRAY( MPOS ) = ARRAY( MPOS ) + HMINV(1,NPOS)*VOLM(M)
                ENDIF
 1110   CONTINUE
C
        DO 1160  M = 1 , NMAT
        MPOS       = MATXRG(M)
        IF(MPOS.LE.0) GO TO 1160
C ***  ABSORBER CASE
        IF(ARRAY(MPOS).LE.0.0) THEN
           MTYPX(MPOS) = 2
           SUM        = 0.0
           DO 1120 IT = 2 , ITLAST + 1
           DELDAY     =  DAYS(IT) - DAYS(IT-1)
           DELSEC     =  DELDAY * 60.0 * 60.0 * 24.00
CM         SUM        =  SUM +  ABSRAT(IT,M)*DELSEC
           SUM        =  SUM +  ABSRAT(IT-1,M)*DELSEC
CDEL       EXPSX(IT,MPOS) = EXPSX(IT-1,MPOS) + SUM*VOLM(M)
           EXPSX(IT,MPOS) = EXPSX(IT,MPOS) + SUM*VOLM(M)
C
CM         IF(IBEDIT.EQ.-2) THEN
CM      WRITE(NOUT2,*) ' ** M ABSRAT(IT-1,M) DELSEC SUM VOLM(M) EXPSX*'
CM      WRITE(NOUT2,*)      M,ABSRAT(IT-1,M),DELSEC,SUM,VOLM(M),
CM   1                      EXPSX(IT,MPOS)
CM         ENDIF
C
 1120      CONTINUE
           ENDIF
C ***  FUEL CASE
      IF(ARRAY(MPOS).GT.0.0) THEN
        MTYPX(MPOS)    = 1
        NPOS           = MATDPL(M)
        IF(NPOS.GT.0) THEN
            WTSAVE     = HMINV(1,NPOS)*VOLM(M)
            DO 1140 IT = 1 , ITLAST + 1
            EXPSX(IT,MPOS) = EXPSX(IT,MPOS) + EXPSZN(IT,NPOS)*WTSAVE
 1140       CONTINUE
            ENDIF
        ENDIF
C
 1160   CONTINUE
C
        DO 1200  N = 1 , NXR
        WTSAVE     = ARRAY(N)
        IF(WTSAVE.GT.0.0) THEN
                  DO 1180  IT = 1 , ITLAST + 1
                  EXPSX(IT,N) = EXPSX(IT,N) / WTSAVE
 1180             CONTINUE
                  ELSE
                  DO 1190  IT = 1 , ITLAST + 1
                  EXPSX(IT,N) = EXPSX(IT,N) / VOLX(N)
 1190             CONTINUE
                  ENDIF
 1200   CONTINUE
C
C *** EDIT NUCLIDE NUMBER DENSITY
C
        LENG   = NOWSTP*NTNUC*NXR
        CALL  CLEA  ( DENSX , LENG , 0.0 )
        DO 1240 M = 1 , NMAT
        MPOS      = MATXRG(M)
        NPOS      = MATDPL(M)
        IF(MPOS.GT.0.AND.NPOS.GT.0) THEN
            VOL        =  VOLM(M)
            DO 1220 J  = 1 , NTNUC
            DO 1220 IT = 1 , ITLAST + 1
            DENSX(IT,J,MPOS) = DENSX(IT,J,MPOS) + DNSITY(IT,J,NPOS)*VOL
 1220       CONTINUE
            ENDIF
 1240   CONTINUE
C
        DO 1300 N = 1 , NXR
CM      WRITE(6,*) ' *** X-REG NO & VOL : ',N,VOLX(N)
        IF(VOLX(N).LE.0.0) GO TO 1300
CM      RATIO     = 1.0000 / VOLX(N)
            DO 1280 J  = 1 , NTNUC
            DO 1280 IT = 1 , ITLAST + 1
CMOD        DENSX(IT,J,MPOS) = DENSX(IT,J,MPOS)*RATIO
            DENSX(IT,J,N   ) = DENSX(IT,J,N   )/VOLX(N)
 1280       CONTINUE
 1300   CONTINUE
C
C  *** EDIT U-235 FRACTION
C
        DO 1400 N = 1  , NXR
        SAVEU5    = DENSX(1,IDU235,N)
        IF(SAVEU5.LE.0.0) GO TO 1400
        DO 1350   IT = 1 , ITLAST + 1
        U5NOW        = DENSX(IT,IDU235,N)
        U235FX(IT,N) =  ( SAVEU5 - U5NOW ) * 100.0 / SAVEU5
 1350   CONTINUE
 1400   CONTINUE
C
C  *** EDIT FISSION ENERGY RELEASE  & FP YIELD DATA
C
       LENGX     = NXR *  NOWSTP
       CALL  CLEA  ( ARRAY  , LENGX , 0.0 )
       CALL  CLEA  ( GAMAVX , LENGX , 0.0 )
       CALL  CLEA  ( YDXEX  , LENGX , 0.0 )
       CALL  CLEA  ( YDIOX  , LENGX , 0.0 )
       CALL  CLEA  ( YDSMX  , LENGX , 0.0 )
       CALL  CLEA  ( YDPMX  , LENGX , 0.0 )
C
       DO 1440 M = 1 , NMAT
       IF(MTYP(M).EQ.0) GO TO 1440
       MPOS      = MATXRG(M)
       NPOS      = MATDPL(M)
       IF(MPOS.LE.0)  GO TO 1440
CDEL   MTYPX(MPOS) = MTYP(M)
C
       DO 1420 IT= 1 , ITLAST
       SUM       = 0.0
       DO 1410 N = 1 , LAPSE
       SUM       = SUM + SIGF(N,IT,M)*FLUXM(N,IT,M)
 1410  CONTINUE
       GAMAVX(IT,MPOS) = GAMAVX(IT,MPOS) + GAMAV(IT,NPOS)*SUM*VOLM(M)
       YDXEX (IT,MPOS) = YDXEX (IT,MPOS) + YDXE (IT,NPOS)*SUM*VOLM(M)
       YDIOX (IT,MPOS) = YDIOX (IT,MPOS) + YDIO (IT,NPOS)*SUM*VOLM(M)
       YDSMX (IT,MPOS) = YDSMX (IT,MPOS) + YDSM (IT,NPOS)*SUM*VOLM(M)
       YDPMX (IT,MPOS) = YDPMX (IT,MPOS) + YDPM (IT,NPOS)*SUM*VOLM(M)
       IPOS            = IT + (MPOS-1)*NOWSTP
       ARRAY (IPOS)    = ARRAY(IPOS)     + SUM*VOLM(M)
 1420  CONTINUE
 1440  CONTINUE
C
       DO 1500  N  = 1 , NXR
       IPOS         = 1 + (N-1)*NOWSTP
       SAVE        = ARRAY(IPOS)
       IF(SAVE.GT.0.0) THEN
                DO 1460   IT = 1 , ITLAST
                IPOS         = IT + (N-1)*NOWSTP
                SAVE2        = ARRAY(IPOS)
                GAMAVX(IT,N) = GAMAVX(IT,N)/SAVE2
                YDXEX (IT,N) = YDXEX (IT,N)/SAVE2
                YDIOX (IT,N) = YDIOX (IT,N)/SAVE2
                YDSMX (IT,N) = YDSMX (IT,N)/SAVE2
                YDPMX (IT,N) = YDPMX (IT,N)/SAVE2
 1460           CONTINUE
C
                ELSE
                MPOS  = 0
                DO 1470 M = 1 , NMAT
CMOD            IF(MATXRG(M).EQ.N)  MPOS = M
                IF(MATXRG(M).EQ.N.AND.MATDPL(M).GT.0)  MPOS = MATDPL(M)
 1470           CONTINUE
                IF(MPOS.GT.0) THEN
                     DO 1480 IT = 1 , ITLAST
CM                   YDXEX (IT,N) = YDXE(IT,MATDPL(MPOS))
CM                   YDIOX (IT,N) = YDIO(IT,MATDPL(MPOS))
CM                   YDSMX (IT,N) = YDSM(IT,MATDPL(MPOS))
CM                   YDPMX (IT,N) = YDPM(IT,MATDPL(MPOS))
                     YDXEX (IT,N) = YDXE(IT,MPOS)
                     YDIOX (IT,N) = YDIO(IT,MPOS)
                     YDSMX (IT,N) = YDSM(IT,MPOS)
                     YDPMX (IT,N) = YDPM(IT,MPOS)
 1480                CONTINUE
                     ENDIF
                ENDIF
 1500  CONTINUE
C
C *** EDIT XE-135 EFFECTIVE MICROSCOPIC CROSS SECTION
C
       LENGX     = NXR *  NOWSTP  * LAPSE
       CALL  CLEA  ( SIGXEX , LENGX , 0.0 )
       CALL  CLEA  ( SIGIOX , LENGX , 0.0 )
       CALL  CLEA  ( SIGSMX , LENGX , 0.0 )
       CALL  CLEA  ( SIGPMX , LENGX , 0.0 )
       CALL  CLEA  ( AFISSX , LENGX , 0.0 )
       CALL  CLEA  ( CFERTX , LENGX , 0.0 )
       CALL  CLEA  ( FLUXX  , LENGX , 0.0 )
       CALL  CLEA  ( VLDEPX , NXR   , 0.0 )
C
       DO 1520 M = 1 , NMAT
       MPOS      = MATXRG(M)
       IF(MPOS.GT.0.AND.MATDPL(M).GT.0) THEN
                                  VLDEPX(MPOS) = VLDEPX(MPOS) + VOLM(M)
                                  ENDIF
C
       IF(MPOS.GT.0) THEN
            SAVE       =  VOLM(M)/VOLX(MPOS)
            DO 1510 IT = 1 , ITLAST
            DO 1510 N  = 1 , LAPSE
            FLUXX(N,IT,MPOS) = FLUXX(N,IT,MPOS) + FLUXM(N,IT,M)*SAVE
 1510       CONTINUE
            ENDIF
 1520  CONTINUE
C****  IF VLDEPX(M).EQ.0 , THEN SET 1.0 FOR AVOIDING ZERO DEVIDE N
       DO 1525 I = 1 , NXR
       IF(VLDEPX(I).LE.0.0)  VLDEPX(I) = 1.00
 1525  CONTINUE
C
       IF(IDXE35.LE.0) GO TO 1601
C
       DO 1540 M = 1 , NMAT
       MPOS      = MATXRG(M)
       NPOS      = MATDPL(M)
       IF(MPOS.GT.0.AND.NPOS.GT.0) THEN
          DO 1530 IT = 1 , ITLAST
          SAVE       = DNSITY(IT,IDXE35,NPOS)
          SAVEX      = DENSX (IT,IDXE35,MPOS)
          IF(SAVEX.LE.0.0) SAVE = 1.000
          DO 1530 N  = 1 , LAPSE
          SIGXEX(N,IT,MPOS) = SIGXEX(N,IT,MPOS) +
     *                     SIGXE (N,IT,NPOS)*SAVE*FLUXM(N,IT,M)*VOLM(M)
 1530     CONTINUE
          ENDIF
 1540     CONTINUE
C   *** IT=1 ==> ZERO DEVIDE ???????
        DO 1600 N  = 1 , NXR
        DO 1600 IT = 1 , ITLAST
        SAVE       = DENSX ( IT , IDXE35 , N ) * VOLX(N)
        IF(SAVE.LE.0.0) SAVE  =  VLDEPX(N)
        DO 1580 I  = 1 , LAPSE
        SIGXEX(I,IT,N) = SIGXEX(I,IT,N)/SAVE/FLUXX(I,IT,N)
 1580   CONTINUE
 1600   CONTINUE
C
C *** EDIT  I-135 EFFECTIVE MICROSCOPIC CROSS SECTION
C
 1601   CONTINUE
       IF(IDI135.LE.0) GO TO 1701
       DO 1640 M = 1 , NMAT
       MPOS      = MATXRG(M)
       NPOS      = MATDPL(M)
       IF(MPOS.GT.0.AND.NPOS.GT.0) THEN
          DO 1630 IT = 1 , ITLAST
          SAVE       = DNSITY(IT,IDI135,NPOS)
          SAVEX      = DENSX (IT,IDI135,MPOS)
          IF(SAVEX.LE.0.0) SAVE = 1.000
          DO 1630 N  = 1 , LAPSE
          SIGIOX(N,IT,MPOS) = SIGIOX(N,IT,MPOS) +
     *                     SIGIO (N,IT,NPOS)*SAVE*FLUXM(N,IT,M)*VOLM(M)
 1630     CONTINUE
          ENDIF
 1640     CONTINUE
C   *** IT=1 ==> ZERO DEVIDE ???????
        DO 1700 N  = 1 , NXR
        DO 1700 IT = 1 , ITLAST
        SAVE       = DENSX ( IT , IDI135 , N ) * VOLX(N)
        IF(SAVE.LE.0.0) SAVE  =  VLDEPX(N)
        DO 1680 I  = 1 , LAPSE
        SIGIOX(I,IT,N)=SIGIOX(I,IT,N)/SAVE/FLUXX(I,IT,N)
 1680   CONTINUE
 1700   CONTINUE
C
C *** EDIT SM-149 EFFECTIVE MICROSCOPIC CROSS SECTION
C
 1701   CONTINUE
       IF(IDSM49.LE.0) GO TO 1801
       DO 1740 M = 1 , NMAT
       MPOS      = MATXRG(M)
       NPOS      = MATDPL(M)
       IF(MPOS.GT.0.AND.NPOS.GT.0) THEN
          DO 1730 IT = 1 , ITLAST
          SAVE       = DNSITY(IT,IDSM49,NPOS)
          SAVEX      = DENSX (IT,IDSM49,MPOS)
          IF(SAVEX.LE.0.0) SAVE = 1.000
          DO 1730 N  = 1 , LAPSE
          SIGSMX(N,IT,MPOS) = SIGSMX(N,IT,MPOS) +
     *                     SIGSM (N,IT,NPOS)*SAVE*FLUXM(N,IT,M)*VOLM(M)
 1730     CONTINUE
          ENDIF
 1740     CONTINUE
C   *** IT=1 ==> ZERO DEVIDE ???????
        DO 1800 N  = 1 , NXR
        DO 1800 IT = 1 , ITLAST
        SAVE       = DENSX ( IT , IDSM49 , N ) * VOLX(N)
        IF(SAVE.LE.0.0) SAVE  =  VLDEPX(N)
        DO 1780 I  = 1 , LAPSE
        SIGSMX(I,IT,N)=SIGSMX(I,IT,N)/SAVE/FLUXX(I,IT,N)
 1780   CONTINUE
 1800   CONTINUE
C
C *** EDIT PM-149 EFFECTIVE MICROSCOPIC CROSS SECTION
C
 1801   CONTINUE
       IF(IDPM49.LE.0) GO TO 1901
       DO 1840 M = 1 , NMAT
       MPOS      = MATXRG(M)
       NPOS      = MATDPL(M)
       IF(MPOS.GT.0.AND.NPOS.GT.0) THEN
          DO 1830 IT = 1 , ITLAST
          SAVE       = DNSITY(IT,IDPM49,NPOS)
          SAVEX      = DENSX (IT,IDPM49,MPOS)
          IF(SAVEX.LE.0.0) SAVE = 1.000
          DO 1830 N  = 1 , LAPSE
          SIGPMX(N,IT,MPOS) = SIGPMX(N,IT,MPOS) +
     *                     SIGPM (N,IT,NPOS)*SAVE*FLUXM(N,IT,M)*VOLM(M)
 1830     CONTINUE
          ENDIF
 1840     CONTINUE
C   *** IT=1 ==> ZERO DEVIDE ???????
        DO 1900 N  = 1 , NXR
        DO 1900 IT = 1 , ITLAST
        SAVE       = DENSX ( IT , IDPM49 , N ) * VOLX(N)
        IF(SAVE.LE.0.0) SAVE  =  VLDEPX(N)
        DO 1880 I  = 1 , LAPSE
        SIGPMX(I,IT,N)=SIGPMX(I,IT,N)/SAVE/FLUXX(I,IT,N)
 1880   CONTINUE
 1900   CONTINUE
C
C *** EDIT FISSILE ABSORPTION MACROSCOPIC X-SECTION
C
 1901   CONTINUE
        DO 1940 M = 1 , NMAT
        MPOS      = MATXRG(M)
        NPOS      = MATDPL(M)
        IF(MPOS.GT.0.AND.NPOS.GT.0) THEN
           DO 1920 IT = 1 , ITLAST
           VOL        = VOLM(M)
           DO 1920 I  = 1 , LAPSE
           AFISSX(I,IT,MPOS) = AFISSX(I,IT,MPOS)
     1                       + AFISSM(I,IT,NPOS)*FLUXM(I,IT,M)*VOL
 1920      CONTINUE
           ENDIF
 1940   CONTINUE
C
        DO 1950 N  = 1 , NXR
        DO 1950 IT = 1 , ITLAST
        DO 1950 I  = 1 , LAPSE
        AFISSX(I,IT,N) = AFISSX(I,IT,N)/FLUXX(I,IT,N)/VOLX(N)
 1950   CONTINUE
C
C *** EDIT FERTILE CAPTURE MACROSCOPIC X-SECTION
C
        DO 1980 M = 1 , NMAT
        MPOS      = MATXRG(M)
        NPOS      = MATDPL(M)
        IF(MPOS.GT.0.AND.NPOS.GT.0) THEN
           DO 1960 IT = 1 , ITLAST
           VOL        = VOLM(M)
           DO 1960 I  = 1 , LAPSE
           CFERTX(I,IT,MPOS) = CFERTX(I,IT,MPOS)
     1                       + CFERTM(I,IT,NPOS)*FLUXM(I,IT,M)*VOL
 1960      CONTINUE
           ENDIF
 1980   CONTINUE
C
        DO 2000 N  = 1 , NXR
        DO 2000 IT = 1 , ITLAST
        DO 2000 I  = 1 , LAPSE
        CFERTX(I,IT,N) = CFERTX(I,IT,N)/FLUXX(I,IT,N)/VOLX(N)
 2000   CONTINUE
C
C  ***  START OUTPUT PROCESS
C
C       STEP 1 ==> CASEBNUP
C
        CALL  CLEA ( ARRAY , LENWRK , 0.0 )
C
        IARRAY( 1) = NOWSTP
        IARRAY( 2) = NTNUC
        IARRAY( 3) = NTDEPZ
         ARRAY( 4) = ST235
         ARRAY( 5) = TWTHVY(1)
        IARRAY( 6) = 0
        IARRAY( 7) = 0
        IARRAY( 8) = 0
        IARRAY( 9) = 0
        IARRAY(10) = 0
        CARRAY(11) = CASEID (1:4)
        CARRAY(12) = STDNUC
        DO 2010  I = 1 ,18
        CARRAY(12+I) = TITLE(I)
 2010   CONTINUE
C
        ISW        = 30
        DO 2015  M = 1 , NMAT
        MPOS       = MATDPL(M)
        IF(MPOS.GT.0) THEN
                      CARRAY(ISW + MPOS) = MTNAME(M) (1:4)
                      ENDIF
 2015   CONTINUE
C
        ISW        = 30 + NTDEPZ
        DO 2020  M = 1 , NMAT
        MPOS       = MATDPL(M)
        IF(MPOS.GT.0) THEN
                      IARRAY(ISW + MPOS) = MTYP(M)
                      ENDIF
 2020   CONTINUE
C
        ISW        = 30 + NTDEPZ*2
        DO 2030  M = 1 , NMAT
        MPOS       = MATDPL(M)
        IF(MPOS.GT.0) THEN
                       ARRAY(ISW + MPOS) = VOLM(M)
                      ENDIF
 2030   CONTINUE
C
        ISW        = 30 + NTDEPZ*3
        DO 2040  I = 1 , NTNUC
        ISW        = ISW + 1
        CARRAY(ISW)= SRACID(I)
 2040   CONTINUE
C
        DO 2050  I = 1 , NOWSTP
        ISW        = ISW + 1
        ARRAY(ISW) = DAYS(I)
 2050   CONTINUE
C
        DO 2060  I = 1 , NOWSTP
        ISW        = ISW + 1
        ARRAY(ISW) = EXPST(I)
 2060   CONTINUE
C
        DO 2070  I = 1 , NOWSTP
        ISW        = ISW + 1
        ARRAY(ISW) = U235F(I)
 2070   CONTINUE
C
        DO 2080  I = 1 , NOWSTP
        ISW        = ISW + 1
        ARRAY(ISW) = AKEFF(I)
 2080   CONTINUE
C
        DO 2090  I = 1 , NOWSTP
        ISW        = ISW + 1
        ARRAY(ISW) = AKINF(I)
 2090   CONTINUE
C
        DO 2100  I = 1 , NOWSTP
        ISW        = ISW + 1
        ARRAY(ISW) = INSCR(I)
 2100   CONTINUE
C
        DO 2110  I = 1 , NOWSTP
        ISW        = ISW + 1
        ARRAY(ISW) = INTCR(I)
 2110   CONTINUE
C
        DO 2120  I = 1 , NOWSTP
        ISW        = ISW + 1
        ARRAY(ISW) = POWERL(I)
 2120   CONTINUE
C
        DO 2140  I = 1 , NOWSTP
        ISW        = ISW + 1
        ARRAY(ISW) = FLXNRM(I)
 2140   CONTINUE
C
        DO 2260  I = 1 , NTDEPZ
        DO 2260  K = 1 , NOWSTP
        ISW        = ISW + 1
        ARRAY(ISW) = POWRZN(K,I)
 2260   CONTINUE
C
        DO 2270  I = 1 , NTDEPZ
        DO 2270  K = 1 , NOWSTP
        ISW        = ISW + 1
        ARRAY(ISW) = EXPSZN(K,I)
 2270   CONTINUE
C
        DO 2280  I = 1 , NTDEPZ
        DO 2280  K = 1 , NOWSTP
        ISW        = ISW + 1
        ARRAY(ISW) = HMINV (K,I)
 2280   CONTINUE
C
        DO 2290  I = 1 , NTDEPZ
        DO 2290  K = 1 , NOWSTP
        ISW        = ISW + 1
        ARRAY(ISW) = GAMAV (K,I)
 2290   CONTINUE
C
        DO 2300  I = 1 , NTDEPZ
        DO 2300  K = 1 , NOWSTP
        ISW        = ISW + 1
        ARRAY(ISW) = YDXE  (K,I)
 2300   CONTINUE
C
        DO 2310  I = 1 , NTDEPZ
        DO 2310  K = 1 , NOWSTP
        ISW        = ISW + 1
        ARRAY(ISW) = YDIO  (K,I)
 2310   CONTINUE
C
        DO 2320  I = 1 , NTDEPZ
        DO 2320  K = 1 , NOWSTP
        ISW        = ISW + 1
        ARRAY(ISW) = YDSM  (K,I)
 2320   CONTINUE
C
        DO 2330  I = 1 , NTDEPZ
        DO 2330  K = 1 , NOWSTP
        ISW        = ISW + 1
        ARRAY(ISW) = YDPM  (K,I)
 2330   CONTINUE
C
        DO 2340  I = 1 , NTDEPZ
        DO 2340  J = 1 , NTNUC
        DO 2340  K = 1 , NOWSTP
        ISW        = ISW + 1
        ARRAY(ISW) = DNSITY(K,J,I)
 2340   CONTINUE
C
        DO 2350  I = 1 , NTDEPZ
        DO 2350  K = 1 , NOWSTP
        DO 2350 NG = 1 , LAPSE
        ISW        = ISW + 1
        ARRAY(ISW) = SIGXE (NG,K,I)
 2350   CONTINUE
C
        DO 2360  I = 1 , NTDEPZ
        DO 2360  K = 1 , NOWSTP
        DO 2360 NG = 1 , LAPSE
        ISW        = ISW + 1
        ARRAY(ISW) = SIGIO (NG,K,I)
 2360   CONTINUE
C
        DO 2370  I = 1 , NTDEPZ
        DO 2370  K = 1 , NOWSTP
        DO 2370 NG = 1 , LAPSE
        ISW        = ISW + 1
        ARRAY(ISW) = SIGSM (NG,K,I)
 2370   CONTINUE
C
        DO 2380  I = 1 , NTDEPZ
        DO 2380  K = 1 , NOWSTP
        DO 2380 NG = 1 , LAPSE
        ISW        = ISW + 1
        ARRAY(ISW) = SIGPM (NG,K,I)
 2380   CONTINUE
C
        LENG       = ISW
        MEMBER = CASEID(1:4) // 'BNUP'
        CALL OVRWRT ( MEMBER , ARRAY , LENG )
C
C ****  STEP 2 ==> X-REGION DATA : MEMBER NAME IS  CASEDNXT
C
        DO 3300  LOP = 1 , NXR
        MEMBER       =  CASEID(1:4) // 'DN0T'
        MEMBER (7:7) =  IDTEMP(LOP) (4:4)
        CALL  CLEA ( ARRAY , LENWRK , 0.0 )
C
        IARRAY( 1) = NOWSTP
        IARRAY( 2) = NTNUC
        IARRAY( 3) = LAPSE
        IARRAY( 4) = LAPSET
        IARRAY( 5) = 0
        IARRAY( 6) = 0
        IARRAY( 7) = 0
        IARRAY( 8) = 0
        IARRAY( 9) = 0
        IARRAY(10) = 0
        CARRAY(11) = CASEID (1:4)
        CARRAY(12) = STDNUC
        IARRAY(13) = MTYPX(LOP)
         ARRAY(14) = VOLX (LOP)
C
        ISW        = 14
        DO 3050  I = 1 , NTNUC
        ISW        = ISW + 1
        CARRAY(ISW)= SRACID(I)
 3050   CONTINUE
C
        DO 3060  I = 1 , NOWSTP
        ISW        = ISW + 1
        ARRAY(ISW) = POWRX(I,LOP)
 3060   CONTINUE
C
        DO 3070  I = 1 , NOWSTP
        ISW        = ISW + 1
        ARRAY(ISW) = EXPSX(I,LOP)
 3070   CONTINUE
C
        DO 3080  I = 1 , NOWSTP
        ISW        = ISW + 1
        ARRAY(ISW) = U235FX(I,LOP)
 3080   CONTINUE
C
        DO 3090  I = 1 , NOWSTP
        ISW        = ISW + 1
        ARRAY(ISW) = HMINVX(I,LOP)
 3090   CONTINUE
C
        DO 3100  I = 1 , NOWSTP
        ISW        = ISW + 1
        ARRAY(ISW) = GAMAVX(I,LOP)
 3100   CONTINUE
C
        DO 3110  I = 1 , NOWSTP
        ISW        = ISW + 1
        ARRAY(ISW) = YDXEX (I,LOP)
 3110   CONTINUE
C
        DO 3120  I = 1 , NOWSTP
        ISW        = ISW + 1
        ARRAY(ISW) = YDIOX (I,LOP)
 3120   CONTINUE
C
        DO 3130  I = 1 , NOWSTP
        ISW        = ISW + 1
        ARRAY(ISW) = YDSMX (I,LOP)
 3130   CONTINUE
C
        DO 3140  I = 1 , NOWSTP
        ISW        = ISW + 1
        ARRAY(ISW) = YDPMX (I,LOP)
 3140   CONTINUE
C
        DO 3150  J = 1 , NTNUC
        DO 3150  I = 1 , NOWSTP
        ISW        = ISW + 1
        ARRAY(ISW) = DENSX(I,J,LOP)
 3150   CONTINUE
C
        DO 3160  I = 1 , NOWSTP
        DO 3160 NG = 1 , LAPSE
        ISW        = ISW + 1
        ARRAY(ISW) = AFISSX(NG,I,LOP)
 3160   CONTINUE
C
        DO 3170  I = 1 , NOWSTP
        DO 3170 NG = 1 , LAPSE
        ISW        = ISW + 1
        ARRAY(ISW) = CFERTX(NG,I,LOP)
 3170   CONTINUE
C
        DO 3180  I = 1 , NOWSTP
        DO 3180 NG = 1 , LAPSE
        ISW        = ISW + 1
        ARRAY(ISW) = SIGXEX(NG,I,LOP)
 3180   CONTINUE
C
        DO 3190  I = 1 , NOWSTP
        DO 3190 NG = 1 , LAPSE
        ISW        = ISW + 1
        ARRAY(ISW) = SIGIOX(NG,I,LOP)
 3190   CONTINUE
C
        DO 3200  I = 1 , NOWSTP
        DO 3200 NG = 1 , LAPSE
        ISW        = ISW + 1
        ARRAY(ISW) = SIGSMX(NG,I,LOP)
 3200   CONTINUE
C
        DO 3210  I = 1 , NOWSTP
        DO 3210 NG = 1 , LAPSE
        ISW        = ISW + 1
        ARRAY(ISW) = SIGPMX(NG,I,LOP)
 3210   CONTINUE
C
        LENG       = ISW
        CALL OVRWRT ( MEMBER , ARRAY , LENG )
 3300   CONTINUE
C
C ***   EIDT X-REGION ABSORPTION RATE
C
        CALL  CLEA ( ABSXRG ,  NOWSTP*NTNUC*NXR , 0.0 )
C
       DO 3500 M = 1 , NMAT
       MPOS      = MATXRG(M)
       NPOS      = MATDPL(M)
       IF(MPOS*NPOS.EQ.0) GO TO 3500
            DO 3450  IB = 1 , NOWSTP - 1
            DO 3440  I  = 1 , NTNUC
            ABSXRG(IB,I,MPOS) = ABSXRG(IB,I,MPOS) + ABSMAT(IB,I,NPOS)
 3440       CONTINUE
 3450       CONTINUE
 3500  CONTINUE
C
C ***   PRINT OUT PROCESS ****
C
       CALL        BURNPR ( NOWSTP , IBEDIT , LAPSE  , MATDPL , NMAT   ,
     1                      NXR    , VOLM   , VOLX   , TITLE  , CASEID ,
     2                      NTNUC  , NTDEPZ ,          TWTHVY , STDNUC ,
     3                      LAPSET , NOUT2  , MTYP   , SRACID , GAMAV  ,
     4                      YDXE   , YDIO   , YDSM   , YDPM   , DNSITY ,
     5                      SIGXE  , SIGIO  , SIGSM  , SIGPM  , MTYPX  ,
     6                      POWRX  , EXPSX  , U235FX , HMINVX , GAMAVX ,
     7                      YDXEX  , YDIOX  , YDSMX  , YDPMX  , DENSX  ,
     8                      SIGXEX , SIGIOX , SIGSMX , SIGPMX , AFISSX ,
     9                      CFERTX , FLUXX  , AFISSM , CFERTM , FLUXM  ,
     A                      NTFISS , LASTFP , MTNAME , ABSMAT , ABSXRG )
C
C ***   END OF PROCESS ****
C
        RETURN
        END
