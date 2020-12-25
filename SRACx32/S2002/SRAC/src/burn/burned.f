       SUBROUTINE  BURNED ( NOWSTP , IBEDIT , LAPSE  , MATDPL , MXNUC ,
     1                      NMAT   , NISO   , VOLM   , FLUXMB , MTYP   ,
     2                      NTNUC  , NTDEPZ , DNOLD  , DNNEW  , IDU235 ,
     3                      POWERL , EVTOJ  , REACEV , MATXRG , SRACID ,
     4                      NTFISS , MAXMT3 , ABOGA  , AMASS  , GAM    ,
     5                      ST235  , U5FNOW , DAYNOW , EXPNOW , AKEFF  ,
     6                      AKINF  , IDXE35 , IDI135 , IDSM49 , IDPM49 ,
     7                      POWMAT , NFIS   , NFER   , NAMFIS , NAMFER ,
     8                      IFISDN , IFRTDN , FISFCT , FRTFCT , LISO   ,
     9                      SSCXE5 , SSCI35 , SSCSM9 , SSCPM9 , CCB    ,
     A               CCX   ,DNSITY , COEFLX , AFISSM , CFERTM , FACNRM ,
     B                      SIGC   , SIGF   , SIGA   , RAMDA  ,
     C                      SSA1B  , ARRAY  , IARRAY , LENWRK , MEMBER ,
     D                      CASEID , FLUXM1 , CUMMWD , FLXNRM , CUMWDM ,
     E                      EXPSZN , NOUT2  , MXDEPL , MXZONE ,
     F                      YLDXE5 ,YLDI35  , YLDSM9 , YLDPM9 ,
     G                      ABSRAT ,GAMAVG  , WTHVYM , WTHVY0 ,
     H                      NXR    ,VOLX    )
C
C
C
       INTEGER*4    NISO(NMAT),MTYP(NMAT),LISO(NMAT)
       INTEGER*4    MATXRG(NMAT)
       REAL*4       VOLM(NMAT)
C
       REAL*4       DNOLD(MXNUC,NTDEPZ)
       REAL*4       DNNEW(MXNUC,NTDEPZ)
       REAL*4       DNSITY(1)
       REAL*4       POWMAT(NMAT),CUMWDM(MXDEPL),EXPSZN(MXDEPL)
C
       REAL*4       REACEV(2,MXNUC),AMASS(MXNUC),RAMDA(MXNUC)
       REAL*4       GAM  (NTFISS,NTNUC)
       REAL*4       FLUXMB(LAPSE,NMAT)
       REAL*4       CCB(LAPSE,MAXMT3,MXNUC,NMAT)
       REAL*4       CCX(LAPSE,MAXMT3,MXNUC,NMAT)
       REAL*4       SSA1B(NTNUC,NTDEPZ)
C
       CHARACTER*4  SRACID(NTNUC)
       CHARACTER*8  MEMBER,CASEID
C
       INTEGER*4    MATDPL(NMAT)
       REAL*4        ARRAY(LENWRK)
       INTEGER*4    IARRAY(LENWRK)
C
       REAL*4       SIGF(LAPSE,NMAT),SIGA(LAPSE,NMAT)
       REAL*4       SIGC(LAPSE,NMAT)
       REAL*4       SSCXE5(LAPSE,NMAT),SSCI35(LAPSE,NMAT)
       REAL*4       SSCSM9(LAPSE,NMAT),SSCPM9(LAPSE,NMAT)
       REAL*4       AFISSM(LAPSE,NMAT),CFERTM(LAPSE,NMAT)
C
       CHARACTER*4  NAMFIS(NFIS),NAMFER(NFER)
       INTEGER*4    IFISDN(NFIS),IFRTDN(NFER)
C
       REAL*4       FISFCT(NFIS),FRTFCT(NFER)
       REAL*4       FLUXM1(NMAT)
       REAL*4       YLDXE5(MXZONE),YLDI35(MXZONE)
       REAL*4       YLDSM9(MXZONE),YLDPM9(MXZONE)
       REAL*4       ABSRAT(MXZONE),GAMAVG(MXZONE)
       REAL*4       WTHVYM(MXZONE),WTHVY0(MXZONE)
       REAL*4       VOLX(NXR)
C
C
       LENG  = LAPSE*NMAT
       CALL  CLEA ( SIGF   , LENG  , 0.0 )
       CALL  CLEA ( SIGA   , LENG  , 0.0 )
       CALL  CLEA ( SIGC   , LENG  , 0.0 )
       CALL  CLEA ( AFISSM , LENG  , 0.0 )
       CALL  CLEA ( CFERTM , LENG  , 0.0 )
       CALL  CLEA ( SSCXE5 , LENG  , 0.0 )
       CALL  CLEA ( SSCI35 , LENG  , 0.0 )
       CALL  CLEA ( SSCSM9 , LENG  , 0.0 )
       CALL  CLEA ( SSCPM9 , LENG  , 0.0 )
       CALL  CLEA ( YLDXE5 , MXZONE, 0.0 )
       CALL  CLEA ( YLDI35 , MXZONE, 0.0 )
       CALL  CLEA ( YLDSM9 , MXZONE, 0.0 )
       CALL  CLEA ( YLDPM9 , MXZONE, 0.0 )
       CALL  CLEA ( GAMAVG , MXZONE, 0.0 )
       CALL  CLEA ( ABSRAT , MXZONE, 0.0 )
       CALL  CLEA ( WTHVYM , MXZONE, 0.0 )
       CALL  CLEA ( WTHVY0 , MXZONE, 0.0 )
C **** SET U5 BURNUP FRACTION
       U5FNOW      = 0.0
C
       DO 100   M  = 1 , NMAT
       MPOS        = MATDPL(M)
       IF(MPOS.LE.0) GO TO 100
       IF(IDU235.GT.0.AND.MATXRG(M).GT.0) THEN
                       U5FNOW = U5FNOW + VOLM(M)*DNNEW(IDU235,MPOS)
                       ENDIF
       WTSAV1      = 0.0
       WTSAV2      = 0.0
       DO  50   I  = 1 , NTFISS
       WTSAV1      = WTSAV1 +  DNOLD(I,MPOS)*AMASS(I)
       WTSAV2      = WTSAV2 +  DNNEW(I,MPOS)*AMASS(I)
   50  CONTINUE
CMOD   WTHVYM(M) =  WTSAV2*VOLM(M)*1.000E-6/ABOGA
       WTHVYM(M) =  WTSAV2*        1.000E-6/ABOGA
       WTHVY0(M) =  WTSAV1*        1.000E-6/ABOGA
  100  CONTINUE
C
       IF(IBEDIT.GT.2) THEN
       WRITE(NOUT2,*) ' ** WTHVYM0 : ',(WTHVY0(M),M=1,NMAT)
       WRITE(NOUT2,*) ' ** WTHVYM  : ',(WTHVYM(M),M=1,NMAT)
       WRITE(NOUT2,*) ' ** CUMWDM  : ',(CUMWDM(M),M=1,MXDEPL)
       WRITE(NOUT2,*) ' ** EXPSZN  : ',(EXPSZN(M),M=1,MXDEPL)
                       ENDIF
C
       U5FNOW      = U5FNOW*1.000E+24
       IF(ST235.GT.0.0)   U5FNOW = (ST235-U5FNOW)*100.0/ST235
C **** SET MACROSCOPIC X-SECTION
       DO 200    M = 1 , NMAT
       IF(NISO  (M).LE.0) GO TO 200
CDEL   IF(MATXRG(M).LE.0) GO TO 200
       DO 150    I = 1 , NISO(M)
       DNTEMP      = DNSITY(LISO(M)+I-1)
       DO 150    N = 1 , LAPSE
       SIGC(N,M)   = SIGC(N,M) + DNTEMP*CCB(N,1,I,M)
       SIGF(N,M)   = SIGF(N,M) + DNTEMP*CCB(N,2,I,M)
  150  CONTINUE
       DO 170    N = 1 , LAPSE
       SIGA(N,M)   = SIGC(N,M) + SIGF(N,M)
  170  CONTINUE
  200  CONTINUE
C **** SET MICROSCOPIC X-SECTION FOR XE135,I135,SM149,PM149
       DO 300    M = 1 , NMAT
       IF(NISO  (M).LE.0) GO TO 300
CDEL   IF(MATXRG(M).LE.0) GO TO 300
       IF(MATDPL(M).LE.0) GO TO 300
       IF(IDXE35.GT.0) THEN
                       DO 210 I = 1 , LAPSE
                       SSCXE5(I,M) = CCX(I,1,IDXE35,M)
  210                  CONTINUE
                       ENDIF
       IF(IDI135.GT.0) THEN
                       DO 220 I = 1 , LAPSE
                       SSCI35(I,M) = CCX(I,1,IDI135,M)
  220                  CONTINUE
                       ENDIF
       IF(IDSM49.GT.0) THEN
                       DO 230 I = 1 , LAPSE
                       SSCSM9(I,M) = CCX(I,1,IDSM49,M)
  230                  CONTINUE
                       ENDIF
       IF(IDPM49.GT.0) THEN
                       DO 240 I = 1 , LAPSE
                       SSCPM9(I,M) = CCX(I,1,IDPM49,M)
  240                  CONTINUE
                       ENDIF
  300  CONTINUE
C
C **** SET FISSILE ABSORPTION X-SECTION
C
       FISABS      = 0.0
       FDECAY      = 0.0
       DO 400    M = 1 , NMAT
       MPOS        = MATDPL(M)
       IF(MPOS.LE.0) GO TO 400
       MMK         = NISO(M)
       DO 330  ISO = 1 , NFIS
       DO 310  J   = 1 , NTNUC
       IF(NAMFIS(ISO)(1:3).EQ.SRACID(J)(2:4)) THEN
                           JPOS = J
                           GO TO 311
                           ENDIF
  310  CONTINUE
       GO TO 330
  311  CONTINUE
       DNTEMP  =  DNOLD(JPOS,MPOS)
       MT      = 0
       IF(NAMFIS(ISO)(4:4).EQ.'F')  MT = 2
       IF(NAMFIS(ISO)(4:4).EQ.'C')  MT = 1
       IF(NAMFIS(ISO)(4:4).EQ.'A')  MT = 4
       IF(NAMFIS(ISO)(4:4).EQ.'P')  MT = 6
       IF(NAMFIS(ISO)(4:4).EQ.'N')  MT = 3
       IF(NAMFIS(ISO)(4:4).EQ.'D')  MT = 5
       IF(MT.EQ.0)  GO TO  330
C
       IF(MT.EQ.5.AND.MATXRG(M).GT.0) THEN
             FDECAY = FDECAY  + 1.00E+24*RAMDA(JPOS)*DNTEMP*VOLM(M)
             GO TO 330
             ENDIF
C
       IF(IFISDN(ISO).EQ.0) THEN
         DO 315 N = 1 , LAPSE
         AFISSM(N,M) = AFISSM(N,M) + CCX(N,MT,JPOS,M)*FISFCT(ISO)
  315    CONTINUE
C
         ELSE
         DO 320 N = 1 , LAPSE
         AFISSM(N,M) = AFISSM(N,M) + CCX(N,MT,JPOS,M)*FISFCT(ISO)*DNTEMP
  320    CONTINUE
         ENDIF
C
  330    CONTINUE
         IF(MATXRG(M).LE.0) GO TO 400
         DO 350 N = 1 , LAPSE
         FISABS   = FISABS + AFISSM(N,M)*FLUXMB(N,M)*VOLM(M)*COEFLX
C
      IF(IBEDIT.GT.2) THEN
      WRITE(NOUT2,*) '** NG AFISSM FLUXM: ',N,AFISSM(N,M),FLUXMB(N,M)
     1                                     ,COEFLX,FISABS
                      ENDIF
  350    CONTINUE
  400    CONTINUE
C
C **** SET FERTIEL CAPTURE X-SECTION
C
       CDECAY      = 0.0
       FRTCAP      = 0.0
       DO 500    M = 1 , NMAT
       MPOS        = MATDPL(M)
       IF(MPOS.LE.0) GO TO 500
       MMK         = NISO(M)
       DO 430  ISO = 1 , NFER
       DO 410  J   = 1 , NTNUC
       IF(NAMFER(ISO)(1:3).EQ.SRACID(J)(2:4)) THEN
                           JPOS = J
                           GO TO 411
                           ENDIF
  410  CONTINUE
       GO TO 430
  411  CONTINUE
       DNTEMP  =  DNOLD(JPOS,MPOS)
       MT      = 0
       IF(NAMFER(ISO)(4:4).EQ.'F')  MT = 2
       IF(NAMFER(ISO)(4:4).EQ.'C')  MT = 1
       IF(NAMFER(ISO)(4:4).EQ.'A')  MT = 4
       IF(NAMFER(ISO)(4:4).EQ.'P')  MT = 6
       IF(NAMFER(ISO)(4:4).EQ.'N')  MT = 3
       IF(NAMFER(ISO)(4:4).EQ.'D')  MT = 5
       IF(MT.EQ.0)  GO TO 430
C
       IF(MT.EQ.5.AND.MATXRG(M).GT.0) THEN
             CDECAY = CDECAY  + 1.00E+24*RAMDA(JPOS)*DNTEMP*VOLM(M)
             GO TO 430
             ENDIF
C
       IF(IFRTDN(ISO).EQ.0) THEN
         DO 415 N = 1 , LAPSE
         CFERTM(N,M) = CFERTM(N,M) + CCX(N,MT,JPOS,M)*FRTFCT(ISO)
  415    CONTINUE
C
         ELSE
         DO 420 N = 1 , LAPSE
         CFERTM(N,M) = CFERTM(N,M) + CCX(N,MT,JPOS,M)*FRTFCT(ISO)*DNTEMP
  420    CONTINUE
         ENDIF
C
  430    CONTINUE
         IF(MATXRG(M).LE.0) GO TO 500
         DO 450 N = 1 , LAPSE
         FRTCAP   = FRTCAP + CFERTM(N,M)*FLUXMB(N,M)*VOLM(M)*COEFLX
      IF(IBEDIT.GT.2) THEN
      WRITE(NOUT2,*) '** NG CFERTM FLUXM: ',N,CFERTM(N,M),FLUXMB(N,M)
     1                                     ,COEFLX,FRTCAP
                      ENDIF
  450    CONTINUE
  500    CONTINUE
C
C **** SET ABSORPTION RATE
C
       DO 600    M = 1 , NMAT
       IF(NISO  (M).LE.0) GO TO 600
CDEL   IF(MATXRG(M).LE.0) GO TO 600
       SUMRAT      = 0.0
       DO 550    N = 1 , LAPSE
       SUMRAT      =  SUMRAT +  SIGA(N,M)*FLUXMB(N,M)
C
CM    IF(IBEDIT.EQ.-2) THEN
CM    WRITE(NOUT2,*) '** M N SIGA FLUXM : ',M,N,SIGA(N,M),FLUXMB(N,M)
CM   1              ,COEFLX,SUMRAT,SUMRAT*COEFLX
CM    ENDIF
C
  550  CONTINUE
       ABSRAT(M)   =  SUMRAT*COEFLX
  600  CONTINUE
C
C **** CALCULATE AVERAGE GAMMA DATA
C
       DO 700    M = 1 , NMAT
       MPOS        = MATDPL(M)
       IF(NISO  (M).LE.0) GO TO 700
       IF(MPOS     .LE.0) GO TO 700
       SUM1        = 0.0
       SUM2        = 0.0
       DO 650  ISO = 1 , NTFISS
CMOD   DNTEMP      = DNSITY(LISO(M)+ISO-1)
       DNTEMP      = DNOLD(ISO,MPOS)
       GAMISO      = REACEV(1,ISO)
       DO 650    N = 1 , LAPSE
       SUM1        = SUM1 +  DNTEMP*CCX(N,2,ISO,M)*FLUXMB(N,M)
       SUM2        = SUM2 +  DNTEMP*CCX(N,2,ISO,M)*FLUXMB(N,M)*GAMISO
  650  CONTINUE
       IF(SUM1.GT.0.0)  GAMAVG(M) = SUM2/SUM1
  700  CONTINUE
C
C  *** CALCULATE AVERAGE XE-135 YIELD DATA
C
       IF(IDXE35.LE.0) GO TO 801
       DO 800   M = 1 , NMAT
       YLDXE5(M)  = GAM(IDU235,IDXE35)
       IF(MTYP(M).NE.1) GO TO 800
       MPOS       = MATDPL(M)
       SUM1       = 0.0
       SUM2       = 0.0
       DO 750 ISO = 1 , NTFISS
       DNTEMP     = DNOLD(ISO,MPOS)
       YLDTMP     = GAM  (ISO,IDXE35)
       DO 740   N = 1 , LAPSE
       SUM1       = SUM1 + DNTEMP*CCX(N,2,ISO,M)*FLUXMB(N,M)
       SUM2       = SUM2 + DNTEMP*CCX(N,2,ISO,M)*FLUXMB(N,M)*YLDTMP
  740  CONTINUE
  750  CONTINUE
       IF(SUM1.GT.0.0)  YLDXE5(M) = SUM2/SUM1
  800  CONTINUE
  801  CONTINUE
C
C  *** CALCULATE AVERAGE I-135 YIELD DATA
C
       IF(IDI135.LE.0) GO TO 901
       DO 900   M = 1 , NMAT
       YLDI35(M)  = GAM(IDU235,IDI135)
       IF(MTYP(M).NE.1) GO TO 900
       MPOS       = MATDPL(M)
       SUM1       = 0.0
       SUM2       = 0.0
       DO 850 ISO = 1 , NTFISS
       DNTEMP     = DNOLD(ISO,MPOS)
       YLDTMP     = GAM  (ISO,IDI135)
       DO 840   N = 1 , LAPSE
       SUM1       = SUM1 + DNTEMP*CCX(N,2,ISO,M)*FLUXMB(N,M)
       SUM2       = SUM2 + DNTEMP*CCX(N,2,ISO,M)*FLUXMB(N,M)*YLDTMP
  840  CONTINUE
  850  CONTINUE
       IF(SUM1.GT.0.0)  YLDI35(M) = SUM2/SUM1
  900  CONTINUE
  901  CONTINUE
C
C  *** CALCULATE AVERAGE SM-149 YIELD DATA
C
       IF(IDSM49.LE.0) GO TO 1001
       DO 1000  M = 1 , NMAT
       YLDSM9(M)  = GAM(IDU235,IDSM49)
       IF(MTYP(M).NE.1) GO TO 1000
       MPOS       = MATDPL(M)
       SUM1       = 0.0
       SUM2       = 0.0
       DO 950 ISO = 1 , NTFISS
       DNTEMP     = DNOLD(ISO,MPOS)
       YLDTMP     = GAM  (ISO,IDSM49)
       DO 940   N = 1 , LAPSE
       SUM1       = SUM1 + DNTEMP*CCX(N,2,ISO,M)*FLUXMB(N,M)
       SUM2       = SUM2 + DNTEMP*CCX(N,2,ISO,M)*FLUXMB(N,M)*YLDTMP
  940  CONTINUE
  950  CONTINUE
       IF(SUM1.GT.0.0)  YLDSM9(M) = SUM2/SUM1
 1000  CONTINUE
 1001  CONTINUE
C
C  *** CALCULATE AVERAGE PM-149 YIELD DATA
C
       IF(IDPM49.LE.0) GO TO 1101
       DO 1100  M = 1 , NMAT
       YLDPM9(M)  = GAM(IDU235,IDPM49)
       IF(MTYP(M).NE.1) GO TO 1100
       MPOS       = MATDPL(M)
       SUM1       = 0.0
       SUM2       = 0.0
       DO 1050 ISO= 1 , NTFISS
       DNTEMP     = DNOLD(ISO,MPOS)
       YLDTMP     = GAM  (ISO,IDPM49)
       DO 1040  N = 1 , LAPSE
       SUM1       = SUM1 + DNTEMP*CCX(N,2,ISO,M)*FLUXMB(N,M)
       SUM2       = SUM2 + DNTEMP*CCX(N,2,ISO,M)*FLUXMB(N,M)*YLDTMP
 1040  CONTINUE
 1050  CONTINUE
       IF(SUM1.GT.0.0)  YLDPM9(M) = SUM2/SUM1
 1100  CONTINUE
C
C **** WRITE BURNUP HISTORY DATA INT0 MACROWORK/MACRO FILE
C      MEMBER NAME  IS 'CASEHTNN'  NN IS NOWSTP
C
 1101  CONTINUE
       MEMBER = CASEID(1:4) // 'HT00'
       WRITE(MEMBER(7:8),'(I2.2)') NOWSTP
C
       IARRAY( 1) =  NOWSTP
        ARRAY( 2) =  DAYNOW
        ARRAY( 3) =  EXPNOW
        ARRAY( 4) =  U5FNOW
        ARRAY( 5) =  AKEFF
        IF(AKINF.LE.0.0)  AKINF = AKEFF
        ARRAY( 6) =  AKINF
        ARRAY( 7) =  CUMMWD
        ARRAY( 8) =  POWERL
        ARRAY( 9) =  FLXNRM
        ARRAY(10) =  FACNRM
        ARRAY(11) =  FISABS
        ARRAY(12) =  FRTCAP
        ARRAY(13) =  FDECAY
        ARRAY(14) =  CDECAY
        ARRAY(15) = 0.0
        SAVE      =  FISABS + FDECAY
        IF(SAVE.GT.0.0)  ARRAY(15) =  ( FRTCAP + CDECAY ) /  SAVE
        ARRAY(16) = COEFLX
        ARRAY(17) = 0.0
        ARRAY(18) = 0.0
CMOD    ARRAY(19) = 0.0
CMOD    ARRAY(20) = 0.0
        IARRAY(19)= NMAT
        IARRAY(20)= NXR
        ISW       = 20
C
        DO 1110 M = 1 , NMAT
        MPOS      = MATDPL(M)
        IF(MPOS.GT.0) THEN
                      ISW   =  ISW + 1
                      ARRAY(ISW) = POWMAT(M)
                      ENDIF
 1110   CONTINUE
C
        DO 1120 M = 1 , NMAT
        MPOS      = MATDPL(M)
        IF(MPOS.GT.0) THEN
                      ISW   =  ISW + 1
                      ARRAY(ISW) = CUMWDM(MPOS)
                      ENDIF
 1120   CONTINUE
C
        DO 1130 M = 1 , NMAT
        MPOS      = MATDPL(M)
        IF(MPOS.GT.0) THEN
                      ISW   =  ISW + 1
                      ARRAY(ISW) = EXPSZN(MPOS)
                      ENDIF
 1130   CONTINUE
C
        IF(NOWSTP.EQ.1)THEN
                       TWTHVY  = 0.0
                       DO 1140 M = 1 , NMAT
                       MPOS      = MATDPL(M)
                       IF(MPOS.GT.0) THEN
                                     ISW        =  ISW + 1
                                     ARRAY(ISW) = WTHVY0(M)
                                     TWTHVY = TWTHVY + VOLM(M)*WTHVY0(M)
                                     ENDIF
 1140                  CONTINUE
                       ARRAY(17) = TWTHVY
                       ARRAY(18) = ST235
                       ENDIF
C
        DO 1145 M = 1 , NMAT
        MPOS      = MATDPL(M)
        IF(MPOS.GT.0) THEN
                      ISW   =  ISW + 1
                      ARRAY(ISW) = WTHVYM(M)
                      ENDIF
 1145   CONTINUE
C
        DO 1150 M = 1 , NMAT
        MPOS      = MATDPL(M)
        IF(MPOS.GT.0) THEN
                      ISW   =  ISW + 1
                      ARRAY(ISW) = GAMAVG(M)*EVTOJ*1.0000E+6
                      ENDIF
 1150   CONTINUE
C
        DO 1160 M = 1 , NMAT
        MPOS      = MATDPL(M)
        IF(MPOS.GT.0) THEN
                      ISW   =  ISW + 1
                      ARRAY(ISW) = YLDXE5(M)
                      ENDIF
 1160   CONTINUE
C
        DO 1170 M = 1 , NMAT
        MPOS      = MATDPL(M)
        IF(MPOS.GT.0) THEN
                      ISW   =  ISW + 1
                      ARRAY(ISW) = YLDI35(M)
                      ENDIF
 1170   CONTINUE
C
        DO 1180 M = 1 , NMAT
        MPOS      = MATDPL(M)
        IF(MPOS.GT.0) THEN
                      ISW   =  ISW + 1
                      ARRAY(ISW) = YLDSM9(M)
                      ENDIF
 1180   CONTINUE
C
        DO 1190 M = 1 , NMAT
        MPOS      = MATDPL(M)
        IF(MPOS.GT.0) THEN
                      ISW   =  ISW + 1
                      ARRAY(ISW) = YLDPM9(M)
                      ENDIF
 1190   CONTINUE
C
        IF(NOWSTP.EQ.1) THEN
                        DO 1200 M = 1 , NTDEPZ
                        DO 1200 I = 1 , NTNUC
                        ISW       =  ISW + 1
                        ARRAY(ISW)= DNOLD(I,M)
 1200                   CONTINUE
                        ENDIF
C
        DO 1210 M = 1 , NTDEPZ
        DO 1210 I = 1 , NTNUC
        ISW       =  ISW + 1
        ARRAY(ISW)= DNNEW(I,M)
 1210   CONTINUE
C
        DO 1300 M = 1 , NMAT
        MPOS      = MATDPL(M)
        IF(MPOS.LE.0) GO TO 1300
        DO 1250 N = 1 , LAPSE
        ISW       = ISW +1
        ARRAY(ISW)= SSCXE5(N,M)
 1250   CONTINUE
 1300   CONTINUE
C
        DO 1400 M = 1 , NMAT
        MPOS      = MATDPL(M)
        IF(MPOS.LE.0) GO TO 1400
        DO 1350 N = 1 , LAPSE
        ISW       = ISW +1
        ARRAY(ISW)= SSCI35(N,M)
 1350   CONTINUE
 1400   CONTINUE
C
        DO 1500 M = 1 , NMAT
        MPOS      = MATDPL(M)
        IF(MPOS.LE.0) GO TO 1500
        DO 1450 N = 1 , LAPSE
        ISW       = ISW +1
        ARRAY(ISW)= SSCSM9(N,M)
 1450   CONTINUE
 1500   CONTINUE
C
        DO 1600 M = 1 , NMAT
        MPOS      = MATDPL(M)
        IF(MPOS.LE.0) GO TO 1600
        DO 1550 N = 1 , LAPSE
        ISW       = ISW +1
        ARRAY(ISW)= SSCPM9(N,M)
 1550   CONTINUE
 1600   CONTINUE
C
        DO 1700 M = 1 , NMAT
        MPOS      = MATDPL(M)
        IF(MPOS.LE.0) GO TO 1700
        DO 1650 N = 1 , LAPSE
        ISW       = ISW +1
        ARRAY(ISW)= AFISSM(N,M)
 1650   CONTINUE
 1700   CONTINUE
C
        DO 1800 M = 1 , NMAT
        MPOS      = MATDPL(M)
        IF(MPOS.LE.0) GO TO 1800
        DO 1750 N = 1 , LAPSE
        ISW       = ISW +1
        ARRAY(ISW)= CFERTM(N,M)
 1750   CONTINUE
 1800   CONTINUE
C
        DO 1900 M = 1 , NMAT
CDEL    MPOS      = MATXRG(M)
CDEL    IF(MPOS.LE.0) GO TO 1900
        ISW       = ISW +1
        ARRAY(ISW)= ABSRAT(M)
 1900   CONTINUE
C
        DO 2000 M = 1 , NMAT
CDEL    IF(MATXRG(M).LE.0) GO TO 2000
        DO 1910 N = 1 , LAPSE
        ISW       = ISW +1
        ARRAY(ISW)= FLUXMB(N,M)*COEFLX
 1910   CONTINUE
        DO 1920 N = 1 , LAPSE
        ISW       = ISW +1
        ARRAY(ISW)= SIGF(N,M)
 1920   CONTINUE
        DO 1930 N = 1 , LAPSE
        ISW       = ISW +1
        ARRAY(ISW)= SIGA(N,M)
 1930   CONTINUE
 2000   CONTINUE
C
        DO 2100 M = 1 , NMAT
        MPOS      = MATDPL(M)
        IF(MPOS.LE.0) GO TO 2100
        SAVE      = VOLM(M)*FLUXM1(M)*COEFLX
        DO 2050 I = 1 , NTNUC
        ISW       = ISW +1
        ARRAY(ISW)= SSA1B(I,MPOS)*DNOLD(I,MPOS)*SAVE
 2050   CONTINUE
 2100   CONTINUE
C
        IF(NOWSTP.EQ.1)THEN
                       DO 2200 I = 1 , NXR
                       ISW       = ISW + 1
                       ARRAY(ISW)= VOLX(I)
 2200                  CONTINUE
CM      WRITE(6,*) ' ** MAMBER NOWSTP NXR (BURNED) : ',MEMBER,NOWSTP,NXR
CM      WRITE(6,*) ' ** VOLX : ',VOLX
                       DO 2300   I = 1 , NMAT
                       ISW         = ISW + 1
                       IARRAY(ISW) = MTYP(I)
 2300                  CONTINUE
CM      WRITE(6,*) ' ** NMAT MTYP : ',NMAT,(MTYP(M),M=1,NMAT)
                       ENDIF
C
        IF(IBEDIT.GT.0) THEN
          WRITE(NOUT2,*) ' MEMBER(',MEMBER,') OF LENGTH ',ISW,' WORDS',
     1       ' WAS WRITTEN  TEMPORARY IN MACROWORK/MCRO FILE '
              ENDIF
C
        CALL  OVRWRT( MEMBER , ARRAY , ISW )
C
        RETURN
        END
