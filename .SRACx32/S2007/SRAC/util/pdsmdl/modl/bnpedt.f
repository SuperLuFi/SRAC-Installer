C***********************************************************************
C  PROGRAM TO EDIT MEMBER 'CASE'//BNUP IN MACRO OR MACROWRK FILE       *
C  VERSION : SRAC95 FOR UNIX                                           *
C***********************************************************************
C
      SUBROUTINE BNPEDT (DIRNAM, IOUT, IPRN, MEMNAM, NGC,
     &           NOWSTP, NTNUC, NZON, CASE, STDNUC, TITLE, MTNM, MTYP,
     &           VOLDPZ, DAYS, EXPST, U235F, AKEFF, AKINF, CRINS, CRINT,
     &           POWERL, FLXNRM, POWRZN, EXPSZN, HMINV, RLHT, YDXE,
     &           YDI0, YDSM, YDPM, NUCLID, DENSTY, SIGXE, SIGI0, SIGSM,
     &           SIGPM)
C
C=========================== FOR MAIN ==================================
CDEL  PARAMETER (MAXNGC=20, MAXSTP=35, MAXNUC=110)
CDEL  PARAMETER (MAXZN=50)
      INCLUDE  'PARAMINC'
      DIMENSION   MTYP(MAXZN), VOLDPZ(MAXZN), DAYS(MAXSTP),
     &            EXPST(MAXSTP), U235F(MAXSTP), AKEFF(MAXSTP),
     &            AKINF(MAXSTP), CRINS(MAXSTP), CRINT(MAXSTP),
     &            POWERL(MAXSTP), FLXNRM(MAXSTP),
     &            POWRZN(MAXSTP,MAXZN), EXPSZN(MAXSTP,MAXZN),
     &            HMINV(MAXSTP,MAXZN), RLHT(MAXSTP,MAXZN),
     &            YDXE(MAXSTP,MAXZN), YDI0(MAXSTP,MAXZN),
     &            YDSM(MAXSTP,MAXZN), YDPM(MAXSTP,MAXZN),
     &            DENSTY(MAXSTP,MAXNUC,MAXZN),
     &            SIGXE(MAXNGC,MAXSTP,MAXZN),SIGI0(MAXNGC,MAXSTP,MAXZN),
     &            SIGSM(MAXNGC,MAXSTP,MAXZN),SIGPM(MAXNGC,MAXSTP,MAXZN)
      CHARACTER*72 TITLE
      CHARACTER*4  CASE, STDNUC, NUCLID(MAXNUC)
      CHARACTER*4  MTNM(MAXZN)
      CHARACTER*8  MEMNAM
      CHARACTER*72 DIRNAM
C=======================================================================
CDEL  PARAMETER    (MAXWRK=100000)
      COMMON /WKPDS/ WORK(MAXWRK)
      DIMENSION      IWORK(1)
      EQUIVALENCE  (WORK(1),IWORK(1))
      CHARACTER*8  MEMBER
C-------------------------------INPUT-----------------------------------
C   DIRNAM       : DIRECTORY NAME (A72) OF PDS : /XXX/XXX/MACRO01
C   IOUT         : LOGICAL DEVICE FOR OUTPUT
C   IPRN         : =0(NO PRINT), =1(PRINT OUT IN DEVICE IOUT)
C   MEMNAM       : PDS MEMBER NAME TO EDIT(A8): ____BNUP
C   NGC          : NUMBER OF CONDENSED ENERGY GROUPS
C-------------------------------OUTPUT----------------------------------
C   NOWSTP       : NUMBER OF BURNUP STEPS INCLUDING THE INITIAL STEP
C                  NOWSTP = 1 + NEP(INPUT IN BURNUP)
C   NTNUC        : TOTAL NUMBER OF DEPLETING NUCLIDES
C   NZON         : TOTAL NUMBER OF DEPLETING ZONE(=MATERIAL REGION)
C   CASE         : CASE IDENTIFICATION (A4)
C   STDNUC       : STANDARD NUCLIDE NAME (A4) TO INDICATE FRACTIONAL
C                  BURNED DENSITY (%), DEFALT:XU05
C   TITLE        : COMMENT (A72)
C   MTNM(I)      : MATERIAL NAME (A4) BY DEPLETING ZONE, I=1...NZON
C                  THE FIRST 4-CHARACTERS OF THE MATERIAL NAME IN INPUT
C   MTYP(I)      : MATERIAL TYPE BY DEPLETING ZONE, I=1...NZON
C                  (=1:FISSILE & BURNABLE, =2:NOT FISSILE BUT BURNABLE)
C   VOLDPZ(I)    : VOLUME OF DEPLETING ZONE(I) (CC)
C   NUCLID(K)    : K-TH DEPLETING NUCLIDE NAME (A4), K=1....NTNUC
C                  EX. XU05,XPU9
C   DAYS(J)      : ACCUMULATED BURNUP PERIOD (DAYS) BY STEP(J)
C   EXPST(J)     : ACCUMULATED BURNUP (MWD/T) BY STEP(J)
C   U235F(J)     : FRACTIONAL BURNED DENSITY OF STDNUC (%)
C   AKEFF(J)     : K-EFF BY STEP(J)
C   AKINF(J)     : K-INF BY STEP(J)
C   CRINS(J)     : INSTANTANEOUS CONVERSION RATIO BY STEP(J)
C   CRINT(J)     : INTEGRATED CONVERSION RATIO BY STEP(J)
C   POWERL(J)    : TOTAL POWER IN A UNIT-LENGTH(CM) LATTICE (MWT)
C   FLXNRM(J)    : NORMALIZATION FACTOR OF FLUX LEVEL BY STEP(J)
C   POWRZN(J,I)  : POWER DENSITY (W/CM3) BY STEP(J), ZONE(I)
C   EXPSZN(J,I)  : ACCUMULATED BURNUP (MWD/T) BY STEP(J), ZONE(I)
C                  IF MTYP(I)=2 => ACCUMULATED ABSORPTION RATE(ABS/CM3)
C   HMINV(J,I)   : HEAVY METAL DENSITY (TON/CC) BY STEP(J), ZONE(I)
C   RLHT(J,I)    : RELEASED ENERGY/FISSION (J/FISS) BY STEP(J), ZONE(I)
C   YDXE(J,I)    : FISSION YIELD OF XE-135 BY STEP(J), ZONE(I)
C   YDI0(J,I)    : FISSION YIELD OF I-135 BY STEP(J), ZONE(I)
C   YDSM(J,I)    : FISSION YIELD OF SM-149 BY STEP(J), ZONE(I)
C   YDPM(J,I)    : FISSION YIELD OF PM-149 BY STEP(J), ZONE(I)
C   DENSTY(J,K,I): DENSITY OF NUCLIDE(K) BY STEP(J), ZONE(I)
C   SIGXE(G,J,I) : MICRO ABSORPTION XS OF XE-135 IN GROUP(G) BY STEP(J)
C                  BY ZONE(I)
C   SIGI0(G,J,I) : MICRO ABSORPTION XS OF I-135 IN GROUP(G) BY STEP(J)
C                  BY ZONE(I)
C   SIGSM(G,J,I) : MICRO ABSORPTION XS OF SM-149 IN GROUP(G) BY STEP(J)
C                  BY ZONE(I)
C   SIGPM(G,J,I) : MICRO ABSORPTION XS OF PM-149 IN GROUP(G) BY STEP(J)
C                  BY ZONE(I)
C-----------------------------------------------------------------------
**************
*  ZERO SET  *---------------------------------------------------------
**************
      IF (NGC.GT.MAXNGC) THEN
        WRITE(IOUT,*) ' ERROR(BNPEDT): NUMBER OF ENERGY GROUPS(=', NGC,
     &             ') IS GREATER THAN THE SET VALUE(=', MAXNGC, ')'
        if(iout.ne.6) then
        WRITE(6,*)    ' ERROR(BNPEDT): NUMBER OF ENERGY GROUPS(=', NGC,
     &             ') IS GREATER THAN THE SET VALUE(=', MAXNGC, ')'
        endif
        STOP
      ENDIF
C
      IF (NGC.LE.0) THEN
        WRITE(IOUT,*) ' ERROR(BNPEDT): NUMBER OF ENERGY GROUPS(=', NGC,
     &             ') IS INVALID'
        if(iout.ne.6) then
        WRITE(6,*)    ' ERROR(BNPEDT): NUMBER OF ENERGY GROUPS(=', NGC,
     &             ') IS INVALID'
        endif
        STOP
      ENDIF
C
      DO 10 I=1,MAXWRK
        WORK(I)=0.0
   10 CONTINUE
C
      IF(MEMNAM(5:8).NE.'BNUP') THEN
        WRITE(IOUT,*) ' CAUTION(BNPEDT): THE LAST 4 CHARACTER IS NOT',
     &             ' BNUP , MEMBER NAME = ',MEMNAM
        if(iout.ne.6) then 
        WRITE(6,*)    ' CAUTION(BNPEDT): THE LAST 4 CHARACTER IS NOT',
     &             ' BNUP , MEMBER NAME = ',MEMNAM
        endif
      ENDIF
***************************
*  ----BNUP DATA EDIT     *---------------------------------------------
***************************
      MEMBER=MEMNAM
      CALL PDSIN (DIRNAM,MEMBER,WORK,LENG,IRC,IOUT)
      IF(IRC.NE.0) THEN
        WRITE(IOUT,*) ' ERROR(BNPEDT): PDSIN ERROR, CODE=', IRC
        if(iout.ne.6) then 
        WRITE(6,*)    ' ERROR(BNPEDT): PDSIN ERROR, CODE=', IRC
        endif
        STOP
      ENDIF
      IF (LENG.GT.MAXWRK) THEN
        WRITE(IOUT,*) ' ERROR(BNPEDT):REQUIRED WORK SIZE(=', LENG,
     &             ') IS GREATER THAN THE SET VALUE(=', MAXWRK, ')'
        if(iout.ne.6) then 
        WRITE(6,*)    ' ERROR(BNPEDT):REQUIRED WORK SIZE(=', LENG,
     &             ') IS GREATER THAN THE SET VALUE(=', MAXWRK, ')'
        endif
        STOP
      ENDIF
C------ 1:NOWSTP, 2:NTNUC, 3:NZON -----------------------------
      NOWSTP = IWORK(1)
      NTNUC  = IWORK(2)
      NZON   = IWORK(3)
      IPOS = 10
C
      IF (NOWSTP.GT.MAXSTP) THEN
        WRITE(IOUT,*) ' ERROR(BNPEDT): NUMBER OF BURNUP STEP(=', NOWSTP,
     &             ') IS GREATER THAN THE SET VALUE(=', MAXSTP, ')'
        if(iout.ne.6) then 
        WRITE(6,*)    ' ERROR(BNPEDT): NUMBER OF BURNUP STEP(=', NOWSTP,
     &             ') IS GREATER THAN THE SET VALUE(=', MAXSTP, ')'
        endif
        STOP
      ENDIF
      IF (NTNUC.GT.MAXNUC) THEN
        WRITE(IOUT,*) ' ERROR(BNPEDT): NUMBER OF DEP. NUCLIDES(=',
     &       NTNUC, ') IS GREATER THAN THE SET VALUE(=', MAXNUC, ')'
        if(iout.ne.6) then 
        WRITE(6,*)    ' ERROR(BNPEDT): NUMBER OF DEP. NUCLIDES(=',
     &       NTNUC, ') IS GREATER THAN THE SET VALUE(=', MAXNUC, ')'
        endif
        STOP
      ENDIF
      IF (NZON.GT.MAXZN) THEN
        WRITE(IOUT,*) ' ERROR(BNPEDT): NUMBER OF DEP. ZONE(=', NZON,
     &             ') IS GREATER THAN THE SET VALUE(=', MAXZN, ')'
        if(iout.ne.6) then 
        WRITE(6,*)    ' ERROR(BNPEDT): NUMBER OF DEP. ZONE(=', NZON,
     &             ') IS GREATER THAN THE SET VALUE(=', MAXZN, ')'
        endif
        STOP
      ENDIF
C------ 11:CASE, 12:STDNUC, 13:TITLE ----------------
      WRITE(CASE(1:4),'(A4)') IWORK(IPOS+1)
      WRITE(STDNUC(1:4),'(A4)') IWORK(IPOS+2)
      IPOS = IPOS + 2
      DO 100 I=1,18
        II=(I-1)*4+1
        WRITE(TITLE(II:II+3),'(A4)') IWORK(IPOS+I)
  100 CONTINUE
      IPOS = IPOS + 18
C------ 14:MTNM, 15:MTYP, 16:VOLDPZ --------------------------
      DO 110 I=1,NZON
        WRITE(MTNM(I)(1:4),'(A4)') IWORK(IPOS+I)
        MTYP(I) = IWORK(IPOS+NZON+I)
        VOLDPZ(I) = WORK(IPOS+NZON*2+I)
  110 CONTINUE
      IPOS = IPOS + 3*NZON
C------ 17:NUCLID -----------------------------------
      DO 120 K=1,NTNUC
        IPOS = IPOS + 1
        WRITE(NUCLID(K)(1:4),'(A4)') IWORK(IPOS)
  120 CONTINUE
C------ 18:DAYS, 19:EXPST, 20:U235F, 21:AKEFF, 22:AKINF
C       23:CRINS, 24:CRINT, 25:POWERL, 26:FLXNRM ------------------
      DO 130 J=1,NOWSTP
        DAYS(J)   = WORK(IPOS+J)
        EXPST(J)  = WORK(IPOS+NOWSTP*1+J)
        U235F(J)  = WORK(IPOS+NOWSTP*2+J)
        AKEFF(J)  = WORK(IPOS+NOWSTP*3+J)
        AKINF(J)  = WORK(IPOS+NOWSTP*4+J)
        CRINS(J)  = WORK(IPOS+NOWSTP*5+J)
        CRINT(J)  = WORK(IPOS+NOWSTP*6+J)
        POWERL(J) = WORK(IPOS+NOWSTP*7+J)
        FLXNRM(J) = WORK(IPOS+NOWSTP*8+J)
  130 CONTINUE
      IPOS = IPOS + 9*NOWSTP
C------ 27:POWRZN, 28:EXPSZN, 29:HMINV, 30:RLHT, 31:YDXE,
C       32:YDI0, 33:YDSM, 34:YDPM ---------------------------------
      NZST = NZON*NOWSTP
      DO 140 I=1,NZON
        DO 140 J=1,NOWSTP
          POWRZN(J,I) = WORK(IPOS+NOWSTP*(I-1)+J)
          EXPSZN(J,I) = WORK(IPOS+NZST+NOWSTP*(I-1)+J)
          HMINV(J,I)  = WORK(IPOS+2*NZST+NOWSTP*(I-1)+J)
          RLHT(J,I)   = WORK(IPOS+3*NZST+NOWSTP*(I-1)+J)
          YDXE(J,I)   = WORK(IPOS+4*NZST+NOWSTP*(I-1)+J)
          YDI0(J,I)   = WORK(IPOS+5*NZST+NOWSTP*(I-1)+J)
          YDSM(J,I)   = WORK(IPOS+6*NZST+NOWSTP*(I-1)+J)
          YDPM(J,I)   = WORK(IPOS+7*NZST+NOWSTP*(I-1)+J)
  140 CONTINUE
      IPOS = IPOS + 8*NZST
C------ 35:DENSTY ------------------------------------------------
      DO 150 I=1,NZON
        DO 150 K=1,NTNUC
          DO 150 J=1,NOWSTP
            IPOS = IPOS + 1
            DENSTY(J,K,I) = WORK(IPOS)
  150 CONTINUE
C------ 36:SIGXE -------------------------------------------------
      DO 160 I=1,NZON
        DO 160 J=1,NOWSTP
          DO 160 IG=1,NGC
            IPOS = IPOS + 1
            SIGXE(IG,J,I) = WORK(IPOS)
  160 CONTINUE
C------ 37:SIGI0 -------------------------------------------------
      DO 170 I=1,NZON
        DO 170 J=1,NOWSTP
          DO 170 IG=1,NGC
            IPOS = IPOS + 1
            SIGI0(IG,J,I) = WORK(IPOS)
  170 CONTINUE
C------ 38:SIGSM -------------------------------------------------
      DO 180 I=1,NZON
        DO 180 J=1,NOWSTP
          DO 180 IG=1,NGC
            IPOS = IPOS + 1
            SIGSM(IG,J,I) = WORK(IPOS)
  180 CONTINUE
C------ 39:SIGPM -------------------------------------------------
      DO 190 I=1,NZON
        DO 190 J=1,NOWSTP
          DO 190 IG=1,NGC
            IPOS = IPOS + 1
            SIGPM(IG,J,I) = WORK(IPOS)
  190 CONTINUE
C------ DATA LENGTH CHECK ----------------------
      IF(IPOS.NE.LENG) THEN
        WRITE(IOUT,*) ' CAUTION(BNPEDT): DATA LENGTH IN PDS FILE(=',
     &        LENG, ') IS MISMATCHED WITH EDIT LENGTH(=', IPOS,')'
        if(iout.ne.6) then 
        WRITE(6,*)    ' CAUTION(BNPEDT): DATA LENGTH IN PDS FILE(=',
     &        LENG, ') IS MISMATCHED WITH EDIT LENGTH(=', IPOS,')'
        endif
      ENDIF
***************************
*     PRINT OUT           *--------------------------------------------
***************************
      IF(IPRN.EQ.0) GOTO 9000
      WRITE(IOUT,*)
      WRITE(IOUT,7000)
      WRITE(IOUT,*)
      WRITE(IOUT,*) '    ***** HETEROGENEOUS BURNUP DATA :',
     &              ' MEMBER NAME = ',MEMNAM,' *****'
      WRITE(IOUT,*)
      WRITE(IOUT,'(1X,A,A)')         'CASE    = ',CASE
      WRITE(IOUT,'(1X,A,A)')         'TITLE   = ',TITLE
      WRITE(IOUT,'(1X,A,I3)')        'NOWSTP  =',NOWSTP
      WRITE(IOUT,'(1X,A,I3)')        'NTNUC   =',NTNUC
      WRITE(IOUT,'(1X,A,I3)')        'NZON    =',NZON
      WRITE(IOUT,*)
C
      WRITE(IOUT,6000) (J,J=0,NOWSTP-1)
      WRITE(IOUT,6010) (DAYS(J),J=1,NOWSTP)
      WRITE(IOUT,6020) (EXPST(J),J=1,NOWSTP)
      WRITE(IOUT,6030) STDNUC(2:4),(U235F(J),J=1,NOWSTP)
      WRITE(IOUT,6040) (AKEFF(J),J=1,NOWSTP)
      WRITE(IOUT,6050) (AKINF(J),J=1,NOWSTP)
      WRITE(IOUT,6060) (CRINS(J),J=1,NOWSTP)
      WRITE(IOUT,6070) (CRINT(J),J=1,NOWSTP)
      WRITE(IOUT,6080) (POWERL(J),J=1,NOWSTP)
      WRITE(IOUT,6090) (FLXNRM(J),J=1,NOWSTP)
      WRITE(IOUT,*)
 6000 FORMAT(/' BNUP-STEP  ',10(5X,I2,5X):/(12X,10(5X,I2,5X)))
 6010 FORMAT( ' DAYS       ',1P10E12.5:/(12X,1P10E12.5))
 6020 FORMAT( ' MWD/TON    ',1P10E12.5:/(12X,1P10E12.5))
 6030 FORMAT(1X,A3,'-%',6X,  1P10E12.5:/(12X,1P10E12.5))
 6040 FORMAT( ' K-EFF      ',1P10E12.5:/(12X,1P10E12.5))
 6050 FORMAT( ' K-INF      ',1P10E12.5:/(12X,1P10E12.5))
 6060 FORMAT( ' INS.-CR    ',1P10E12.5:/(12X,1P10E12.5))
 6070 FORMAT( ' INT.-CR    ',1P10E12.5:/(12X,1P10E12.5))
 6080 FORMAT( ' POWER(MW)  ',1P10E12.5:/(12X,1P10E12.5))
 6090 FORMAT( ' FLX-NORM.  ',1P10E12.5:/(12X,1P10E12.5))
C
      DO 2000 IZN=1,NZON
        WRITE(IOUT,*)
        WRITE(IOUT,*) ' ***** DEPLETING ZONE(',IZN,') : ',
     &  'MATERIAL NAME=',MTNM(IZN),' : MATERIAL TYPE=',MTYP(IZN),
     &  ' : VOLUME(CC)=',VOLDPZ(IZN)
        WRITE(IOUT,6000) (J,J=0,NOWSTP-1)
        IF(MTYP(IZN).EQ.2) THEN
          WRITE(IOUT,6200) (EXPSZN(J,IZN),J=1,NOWSTP)
        ELSE
          WRITE(IOUT,6210) (EXPSZN(J,IZN),J=1,NOWSTP)
        ENDIF
        WRITE(IOUT,6220) (POWRZN(J,IZN),J=1,NOWSTP)
        WRITE(IOUT,6230) (HMINV(J,IZN),J=1,NOWSTP)
        WRITE(IOUT,6240) (RLHT(J,IZN),J=1,NOWSTP)
        WRITE(IOUT,6250) (YDXE(J,IZN),J=1,NOWSTP)
        WRITE(IOUT,6260) (YDI0(J,IZN),J=1,NOWSTP)
        WRITE(IOUT,6270) (YDSM(J,IZN),J=1,NOWSTP)
        WRITE(IOUT,6280) (YDPM(J,IZN),J=1,NOWSTP)
 6200 FORMAT( ' ABS./CC    ',1P10E12.5:/(12X,1P10E12.5))
 6210 FORMAT( ' MWD/TON    ',1P10E12.5:/(12X,1P10E12.5))
 6220 FORMAT( ' POW(MW/CC) ',1P10E12.5:/(12X,1P10E12.5))
 6230 FORMAT( ' HM-TON/CC  ',1P10E12.5:/(12X,1P10E12.5))
 6240 FORMAT( ' J-ENG/FIS. ',1P10E12.5:/(12X,1P10E12.5))
 6250 FORMAT( ' XE-135 YD  ',1P10E12.5:/(12X,1P10E12.5))
 6260 FORMAT( ' I -135 YD  ',1P10E12.5:/(12X,1P10E12.5))
 6270 FORMAT( ' SM-149 YD  ',1P10E12.5:/(12X,1P10E12.5))
 6280 FORMAT( ' PM-149 YD  ',1P10E12.5:/(12X,1P10E12.5))
        WRITE(IOUT,'(/A)') ' * NUMBER DENSITY (*E24 N/CC)'
        WRITE(IOUT,6300) (J,J=0,NOWSTP-1)
        WRITE(IOUT,6310)
        DO 1000 K=1,NTNUC
          WRITE(IOUT,6320) K,NUCLID(K),(DENSTY(J,K,IZN),J=1,NOWSTP)
 1000   CONTINUE
 6300 FORMAT(/' BNUP-STEP  ',10(5X,I2,5X):/(12X,10(5X,I2,5X)))
 6310 FORMAT(1X,3(1H-),2X,4('-'),1X,11(1X,10(1H-)))
 6320 FORMAT(1X,I3,2X,A4,1X,1P11E11.4:/(11X,1P11E11.4))
 6330 FORMAT(1X,'STEP:',I3,2X,1P10E11.4:/(11X,1P10E11.4))
C
        WRITE(IOUT,'(/A)') ' * XE-135 MICRO ABSORPTION XS BY GROUP'
        DO 1010 JJ=1,NOWSTP
          WRITE(IOUT,6330) JJ-1,(SIGXE(IG,JJ,IZN),IG=1,NGC)
 1010   CONTINUE
C
        WRITE(IOUT,'(/A)') ' * I -135 MICRO ABSORPTION XS BY GROUP'
        DO 1020 JJ=1,NOWSTP
          WRITE(IOUT,6330) JJ-1,(SIGI0(IG,JJ,IZN),IG=1,NGC)
 1020   CONTINUE
C
        WRITE(IOUT,'(/A)') ' * SM-149 MICRO ABSORPTION XS BY GROUP'
        DO 1030 JJ=1,NOWSTP
          WRITE(IOUT,6330) JJ-1,(SIGSM(IG,JJ,IZN),IG=1,NGC)
 1030   CONTINUE
C
        WRITE(IOUT,'(/A)') ' * PM-149 MICRO ABSORPTION XS BY GROUP'
        DO 1040 JJ=1,NOWSTP
          WRITE(IOUT,6330) JJ-1,(SIGPM(IG,JJ,IZN),IG=1,NGC)
 1040   CONTINUE
 2000 CONTINUE
C
      WRITE(IOUT,*)
      WRITE(IOUT,*)
      WRITE(IOUT,7010)
      WRITE(IOUT,*)
 7000 FORMAT(1H ,'BNPEDT',114(1H=))
 7010 FORMAT(1H ,114(1H=),'BNPEDT')
C
 9000 RETURN
      END
