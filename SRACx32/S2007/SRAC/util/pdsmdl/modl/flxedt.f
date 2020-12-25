C***********************************************************************
C  PROGRAM TO EDIT FLUX FROM FLUX-PDS FILE                             *
C  VERSION : SRAC95 FOR UNIX                                           *
C***********************************************************************
C
      SUBROUTINE FLXEDT(DIRNAM, IOUT, IPRN, MEMNAM, NG, NRR, FLX1, VOLR)
C
C=========================== FOR MAIN ==================================
CDEL  PARAMETER  (MAXNG=107, MAXNR=100)
      INCLUDE  'PARAMINC'
      DIMENSION  FLX1(MAXNG,MAXNR), VOLR(MAXNR)
      CHARACTER*72  DIRNAM
      CHARACTER*8   MEMNAM
C=======================================================================
CDEL  PARAMETER  (MAXWRK=11000)
      COMMON /WKPDS/ WORK(MAXWRK)  
      DIMENSION      IWORK(1)
      EQUIVALENCE  (WORK(1),IWORK(1))
      CHARACTER*8   MEMBER
C-------------------------------INPUT-----------------------------------
C     DIRNAM     : DIRECTORY NAME (A72) OF PDS : /XXX/XXX/FLUX01
C     IOUT       : LOGICAL DEVICE FOR OUTPUT
C     IPRN       : =0(NO PRINT), =1(PRINT OUT IN DEVICE IOUT)
C     MEMNAM     : PDS MEMBER NAME TO EDIT(A8)
C     NG         : NUMBER OF ENERGY GROUPS CORRESPONDING TO INPUT
C                  FLUX MEMBER NAME
C-------------------------------OUTPUT----------------------------------
C     NRR        : NUMBER OF R-REGIONS(IF MEMBER NAME CORRESPONDS TO
C                  X-REGION, NRR=1), IF MEMBER NAME = ____SVOL THEN
C                  NRR = NUMBER OF S-REGION
C     FLX1(G,I)  : NORMAL FLUX DATA (FLUX*DU*VOL) FOR GROUP-G, REGION-I
C                  IF MEMBER NAME CORRESPONDS TO X-REGION, FLX1(G,1)
C                  INDICATES THE X-REGION SPECTRUM
C     VOLR(I)    : VOLUME OF R-REGION WHEN E-TAG = 'F' OR 'T' OR 'A',
C                  VOLUME OF T-REGION WHEN E-TAG = 'S'
C-----------------------------------------------------------------------
*****************************
* SIZE CHECK & ZERO SETTING *-------------------------------------------
*****************************
      IF (NG.GT.MAXNG) THEN
        WRITE(IOUT,*) ' ERROR(FLXEDT): NUMBER OF ENERGY GROUPS(=', NG,
     &             ') IS GREATER THAN THE SET VALUE(=', MAXNG, ')'
        if(iout.ne.6) then 
        WRITE(6,*)    ' ERROR(FLXEDT): NUMBER OF ENERGY GROUPS(=', NG,
     &             ') IS GREATER THAN THE SET VALUE(=', MAXNG, ')'
        endif
        STOP
      ENDIF
      DO 100 I=1,MAXWRK
        WORK(I)=0.0
  100 CONTINUE
***************************
*     EDIT FLUX DATA      *--------------------------------------------
***************************
      IF(MEMNAM(6:8).EQ.'VOL') GOTO 300
      DO 200 J=1,MAXNR
        DO 200 I=1,MAXNG
          FLX1(I,J)=0.0
  200 CONTINUE
      MEMBER = MEMNAM
      CALL PDSIN (DIRNAM,MEMBER,WORK,LENG,IRC,IOUT)
      IF(IRC.NE.0) THEN
        WRITE(IOUT,*) ' ERROR(FLXEDT): PDSIN ERROR, CODE=', IRC
        if(iout.ne.6) then 
        WRITE(6,*)    ' ERROR(FLXEDT): PDSIN ERROR, CODE=', IRC
        endif
        STOP
      ENDIF
      NRR = LENG/NG
      IF (NRR.GT.MAXNR) THEN
        WRITE(IOUT,*) ' ERROR(FLXEDT): NUMBER OF R-REGIONS(=', NRR,
     &             ') IS GREATER THAN THE SET VALUE(=', MAXNR, ')'
        if(iout.ne.6) then 
        WRITE(6,*)    ' ERROR(FLXEDT): NUMBER OF R-REGIONS(=', NRR,
     &             ') IS GREATER THAN THE SET VALUE(=', MAXNR, ')'
        endif
        STOP
      ENDIF
      DO 210 K=1,NG
        DO 210 I = 1,NRR
          FLX1(K,I) = WORK((K-1)*NRR+I)
  210 CONTINUE
      GOTO 1000
***************************
*     REGION VOLUME       *--------------------------------------------
***************************
  300 DO 310 J=1,MAXNR
        VOLR(J)=0.0
  310 CONTINUE
      MEMBER = MEMNAM
      CALL PDSIN (DIRNAM,MEMBER,WORK,LENG,IRC,IOUT)
Cmod by okumura (in eigenvalue cal. caseAVOL should be read)
      IF(IRC.EQ.1) THEN
        MEMBER(5:5) = 'A'
        CALL PDSIN (DIRNAM,MEMBER,WORK,LENG,IRC,IOUT)
        IF(IRC.NE.0) THEN
          WRITE(IOUT,*) ' ERROR(FLXEDT): PDSIN ERROR, CODE=', IRC
          if(iout.ne.6) then 
          WRITE(6,*)    ' ERROR(FLXEDT): PDSIN ERROR, CODE=', IRC
          endif
          STOP
        ENDIF
      ELSEIF(IRC.NE.0) THEN
        WRITE(IOUT,*) ' ERROR(FLXEDT): PDSIN ERROR, CODE=', IRC
        if(iout.ne.6) then 
        WRITE(6,*)    ' ERROR(FLXEDT): PDSIN ERROR, CODE=', IRC
        endif
        STOP
      ENDIF
      NRR = LENG
      DO 320 I=1,NRR
        VOLR(I) = WORK(I)
  320 CONTINUE
***************************
*     PRINT OUT           *--------------------------------------------
***************************
      IF(IPRN.EQ.0) GOTO 9000
      WRITE(IOUT,*)
      WRITE(IOUT,7000)
      WRITE(IOUT,*)
      WRITE(IOUT,*) '    ***** REGION VOLUME : MEMBER NAME = ',MEMNAM,
     &           ' *****'
      WRITE(IOUT,*)
      WRITE(IOUT,*) '    NG  = ',NG
      WRITE(IOUT,*) '    NRR = ',NRR
      WRITE(IOUT,*)
      WRITE(IOUT,6000) (I,I=1,NRR)
      WRITE(IOUT,*)
      WRITE(IOUT,6010) (VOLR(I),I=1,NRR)
      WRITE(IOUT,*)
      WRITE(IOUT,*)
      WRITE(IOUT,7010)
      WRITE(IOUT,*)
      GOTO 9000
 1000 IF(IPRN.EQ.0) GOTO 9000
      WRITE(IOUT,*)
      WRITE(IOUT,7000)
      WRITE(IOUT,*)
      WRITE(IOUT,*) '    ***** FLUX OF MEMBER NAME = ',MEMNAM, ' *****'
      WRITE(IOUT,*)
      WRITE(IOUT,*) '    NG  = ',NG
      WRITE(IOUT,*) '    NRR = ',NRR
      WRITE(IOUT,*)
      WRITE(IOUT,6020) (I,I=1,NRR)
      WRITE(IOUT,*)
        DO 1010 IG = 1, NG
          WRITE(IOUT,6030) IG,(FLX1(IG,I),I=1,NRR)
 1010   CONTINUE
      WRITE(IOUT,*)
      WRITE(IOUT,*)
      WRITE(IOUT,7010)
      WRITE(IOUT,*)
 6000 FORMAT(1H ,7HREGION ,10(4X,I4,4X):/(8X,10(4X,I4,4X)))
 6010 FORMAT(1H ,6X,1P10E12.5:/(7X,1P10E12.5))
 6020 FORMAT(1H ,7H  G/R  ,10(4X,I4,4X):/(8X,10(4X,I4,4X)))
 6030 FORMAT(1H ,I4,2X,1P10E12.5:/(7X,1P10E12.5))
 7000 FORMAT(1H ,'FLXEDT',114(1H=))
 7010 FORMAT(1H ,114(1H=),'FLXEDT')
C
 9000 RETURN
      END
