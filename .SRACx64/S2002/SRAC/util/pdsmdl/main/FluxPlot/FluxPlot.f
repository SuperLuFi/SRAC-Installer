C***********************************************************************
C
C  main program for pdsmdl 
C  FluxPlot : Read Fine Gruop Spectrum in each R-Region or X-Region and
C             Edit Table Data (Histgram Type) for any Plot-Applications
C
C***********************************************************************
C### COPYED FROM ENGEDT/FLXEDT
      INCLUDE 'PARAMINC'
      CHARACTER*72 DIRNAM   
      CHARACTER*8  MEMNAM 
      DIMENSION    EN(MAXNG+1), WT(MAXNG+1)
      DIMENSION    FLX1(MAXNG,MAXNR), VOLR(MAXNR)
C-----------------------------------------------------------------------
      CHARACTER*8  MEMBER
      DIMENSION    DU(MAXNG)
      DIMENSION    VOLM(MAXNR)
C-----------------------------------------------------------------------
      call uioset
C---- read directory name for FLUX file
      read(5,'(a72)',end=9999) dirnam
      IOUT   = 99
      IPLT   = 6
      IPRN   = 1
C
C==== loop on members
 1000 read(5,'(a8)',end=9999) member
      if(member.eq.'        ') goto 9999
C---- read energy structur
      IF(MEMBER(8:8).EQ.'2') THEN
        MEMNAM = 'CONTA002'
      ELSE IF(MEMBER(8:8).EQ.'0') THEN
        MEMNAM = 'CONTA000'
      ELSE
        WRITE(6,*) ' THE LAST TAG OF INPUT MEMBER NAME IS INVALID '
        STOP
      ENDIF
      MEMNAM(5:5)=member(5:5)
C---- set lethergy
      call ENGEDT(DIRNAM, IOUT, IPRN, MEMNAM, NG, WT, EN)
      DO 100 I=1,NG
        DU(I) = LOG(EN(I)/EN(I+1))
  100 CONTINUE
C---- set region volume
C     X-REGION VOLUME IS DETERMINED AS TOTAL OF ALL R-REGION
      MEMNAM = MEMBER(1:4)//'FVOL'
      call FLXEDT(DIRNAM, IOUT, IPRN, MEMNAM, NG, NRR, FLX1, VOLR)
      VOLT=0.0
      DO 200 I=1,NRR
        VOLM(I)=VOLR(I)
        VOLT=VOLT+VOLM(I)
  200 CONTINUE
      IF(MEMBER(7:7).NE.'0') THEN
        VOLM(1)=VOLT
      ENDIF
C---- read neutron spectrum 
      MEMNAM = MEMBER
      call FLXEDT(DIRNAM, IOUT, IPRN, MEMNAM, NG, NRR, FLX1, VOLR)
      DO 300 I = 1,NRR
        DO 300 K = 1,NG
          FLX1(K,I) = FLX1(K,I)/DU(K)/VOLM(I)
  300 CONTINUE
C*********************************
C  WRITE PLOT DATA FOR HISTGRAM  *
C*********************************
      do 500 ir=1,nrr
        write(iplt,*)
        WRITE(iplt,*) ' <<<< NEUTRON SPECTRUM(',MEMBER,') REGION(',
     &             ir,') >>>>'
        WRITE(iplt,6000)
        DO 400 IG = 1,NG
          WRITE(iplt,6100) IG,EN(IG),FLX1(IG,ir)
          WRITE(iplt,6100) IG,EN(IG+1),FLX1(IG,ir)
  400   CONTINUE
  500 CONTINUE
 6000 FORMAT(1X,'GROUP',' ENERGY-B.D. ','   SPECTRUM  ')
 6100 FORMAT(1X,I4,1X,1PE12.5,1X,1PE12.5)
      goto 1000
 9999 STOP
      END
C***********************************************************************
C
C  uiount   : set unformated(0) or formated(1)
C             for each I/O device
C
C***********************************************************************
C
      subroutine uiount(ioform)
      dimension ioform(100)
      do 100 i=1,100
        ioform(i) = -1
  100 continue
      ioform(5)  = 1
      ioform(6)  = 1
      ioform(99) = 1
      return
      end
