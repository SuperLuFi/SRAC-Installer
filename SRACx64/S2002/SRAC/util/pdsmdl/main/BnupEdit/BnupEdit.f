C***********************************************************************
C
C  main program for pdsmdl
C  BnupEdit : Print out Burnup Calculational Results
C             Contents of Member ????BNUP/????DN?T 
C             Print out in the 99th Device
C
C***********************************************************************
C### COPYED FROM ENGEDT/BNPEDT/DNTEDT
      INCLUDE 'PARAMINC'
      CHARACTER*72 DIRNAM
      CHARACTER*8  MEMNAM
      DIMENSION   EN(MAXNG+1), WT(MAXNG+1) 
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
      DIMENSION   POWRX(MAXSTP), EXPSX(MAXSTP), U235FX(MAXSTP),  
     &            HMINVX(MAXSTP), RLHTX(MAXSTP), YDXEX(MAXSTP),   
     &            YDI0X(MAXSTP), YDSMX(MAXSTP), YDPMX(MAXSTP),  
     &            DENSX(MAXSTP,MAXNUC), AFISS(MAXNGC,MAXSTP),  
     &            CFERT(MAXNGC,MAXSTP), SIGXEX(MAXNGC,MAXSTP),  
     &            SIGI0X(MAXNGC,MAXSTP),SIGSMX(MAXNGC,MAXSTP),  
     &            SIGPMX(MAXNGC,MAXSTP)   
      CHARACTER*72 TITLE
      CHARACTER*4  CASE, STDNUC, NUCLID(MAXNUC)
      CHARACTER*4  MTNM(MAXZN)
C
C***********************************************************************
      character*8  member
      character*81 filnam
C
      call uioset
C---- read directory name for MACRO/MACROWRK
      read(5,'(a72)',end=9999) dirnam
C      
C---- read number of condensed enegy groups
      memnam = 'CONTA000'
      call PDSNM(DIRNAM,MEMBER,FILNAM,NLENG,IERR)
      call PDSSR(FILNAM,IEXST)
      if(iexst.eq.1) then
        memnam = 'CONTF000'
      endif
      iout = 99
      iprn = 0
      call ENGEDT(DIRNAM, IOUT, IPRN, MEMNAM, NG, WT, EN)
C
C---- loop on member
C
      iout = 6
      iprn = 1
 1000 read(5,'(a8)',end=9999 ) member
      if(member.eq.'        ') then
        goto 9999
      elseif(member(5:8).eq.'BNUP') then
        memnam = member
        call BNPEDT (DIRNAM, IOUT, IPRN, MEMNAM, ng, 
     &           NOWSTP, NTNUC, NZON, CASE, STDNUC, TITLE, MTNM, MTYP,  
     &           VOLDPZ, DAYS, EXPST, U235F, AKEFF, AKINF, CRINS, CRINT,
     &           POWERL, FLXNRM, POWRZN, EXPSZN, HMINV, RLHT, YDXE, 
     &           YDI0, YDSM, YDPM, NUCLID, DENSTY, SIGXE, SIGI0, SIGSM,
     &           SIGPM)
      elseif(member(5:6).eq.'DN' .and. member(8:8).eq.'T') then
        memnam = member
        call DNTEDT (DIRNAM, IOUT, IPRN, MEMNAM, 
     &           NOWSTP, NTNUC, NGC, NGT, CASE, STDNUC, MTYPX, VOLX,   
     &           POWRX, EXPSX, U235FX, HMINVX, RLHTX, YDXEX, YDI0X,  
     &           YDSMX, YDPMX, NUCLID, DENSX, AFISS, CFERT, SIGXEX,  
     &           SIGI0X, SIGSMX, SIGPMX)
      else
        write(6,*) ' ERROR(MAIN) : MEMBER NAME(',member,') IS INVALID'
      endif
C
      goto 1000
 9999 stop
      end
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
