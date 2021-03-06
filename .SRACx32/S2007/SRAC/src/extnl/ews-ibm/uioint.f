C uioint(originaly ioinit) : initialize the I/O system
C        @(#)ioinit.f 1.8 89/02/09 SMI; from UCB 1.5
C synopsis:
C        logical function uioint (cctl, bzro, apnd, prefix, vrbose,
C                                 istder, ideft, mxunit)
C        logical cctl, bzro, apnd, vrbose
C        character*(*) prefix
C        integer istder, ideft, mxunit
C
C where:
C        cctl        is .true. to turn on fortran-66 carriage control
C        bzro        is .true. to cause blank space to be zero on input
C        apnd        is .true. to open files at their end
C        prefix      is a string defining environment variables to
C                       be used to initialize logical units.
C        vrbose      is .true. if the caller wants output showing the
C                       lu association
C        istder      is a device number of standerd error output
C        ideft       =0 : not open for the device prefix is not defined.
C                    =1 : open by defalt name(temp.nn) if prefix is not
C                         defined and ioform .GE. 0 (except 5,6 istder).
C        mxunit      maximum number of openable i/o devices
C                    mxunit = 64(Sun), 48(HP)
C
C returns:
C        .true. if all went well
C
C --------------------------------------------------------------------
C  David L. Wasley (U.C.Bekeley)
C  Modified by M.Sasaki (The Japan Research Institute Ltd., Tokyo,Japan)
C  Modified by K.Okumura (JAERI) and M.Ido (ITJ) 
C
C  * Make applicable to systems other than BSD UNIX such as HP.
C  * Make applicable to also unformatted files.
C
C     I/O unit # of standard error may be other than 0 (ex. 7 in HP)
C     Function "lnblnk" may not exist.
C     Function "perror" may not exist.
C     Using tabs in program source may cause trouble.
C     REWIND is not allowed for not-opened devices in HP
C     See also the subroutines (uioset.f, uiount.f)      
C----------------------------------------------------------------------
C
        logical function uioint (cctl, bzro, apnd, prefix, vrbose,
     &                           istder, ideft, mxunit)
C
        logical          cctl, bzro, apnd, vrbose
        character*(*)    prefix
C:automatic may be invalid in some systems(SX3)
CDEL    automatic        iok, fenv, ienv, ename, fname, form, blank
        logical          iok, fenv, ienv
CDEL    integer*2        ieof,  ictl,  izro
C:full specification of open parameter is desirable.
CMOD    character        form, blank
        character*16     form, blank
        character*32     ename
        character*256    fname
        character*8      tname
        character*8      torf1, torf2, torf3
C
C###################################################################
C IF (Version of Sun SPARC Fortran Compiler .GE. 3.0) THEN
C       common /__ioiflg/        ieof, ictl, izro
C ELSE 
CDEL    common /ioiflg/          ieof, ictl, izro
C ENDIF
C###################################################################
C
C:ioform <0 not open, = 0 open in unformatted, =1 open in formatted
C ioform is set in program dependent subroutine (uiount)
        parameter        (nounit = 100)
        integer          ioform(nounit)
C
C-------------- Process Start -----------------------------------------
C
        ntunt = 0
        tname = 'temp.nm'
        call uiount(ioform)
C
        if (cctl) then
            ictl = 1
C: SUN Specific file form 'PRINT' may not be supported in other systems.
CMOD        form = 'PRINT'
            form = 'FORMATTED'
        else
            ictl = 0
CMOD        form = 'f'
            form = 'FORMATTED'
        endif
C
        if (bzro) then
            izro = 1
CMOD        blank = 'z'
            blank = 'ZERO'
        else
            izro = 0
CMOD        blank = 'n'
            blank = 'NULL'
        endif
C
CKSK standard I/O device should not open for IBM AIX RS/6000
C    because fort.5, fort.6 will be allocated.
CIBM    open (unit=5, form=form, blank=blank)
CIBM    open (unit=6, form=form, blank=blank)
        ntunt = ntunt + 2
C
        if (apnd) then
            ieof = 1
        else
            ieof = 0
        endif
C
        iok  = .true.
        fenv = .false.
        ienv = .false.
        lp = len (prefix)
C
        if ((lp .gt. 0) .and. (lp .le. 30) .and. (prefix .ne. " ")) then
            ienv = .true.
            nb = index (prefix, " ")
            if (nb .eq. 0) nb = lp + 1
            ename = prefix
            if (vrbose) write (istder, 2002) ename(:nb-1)
C
CMOD        do 200 lu = 0, nounit-1
            do 200 lu = 1, nounit-1
                write (ename(nb:), "(i2.2)") lu
CKSK            call getenv (ename, fname)
                call UGTENV (ename, fname)
C:REWIND is not allowed for not-opened devices in HP
CMOD            if (fname .eq. " ") go to 200
                if (ioform(lu) .lt. 0) go to 200
                if ((lu.eq.5).or.(lu.eq.6).or.(lu.eq.istder)) goto 200
                if ((fname .eq. " ").and.(ideft.ne.0)) then
                  write (tname(6:7), "(i2.2)") lu
                  fname = tname
                endif
                if (fname .eq. " ") go to 200
C
                if (ioform(lu) .eq. 1) then
                  open(unit=lu,file=fname,form='FORMATTED',
     &                 access='SEQUENTIAL', err=100)
                  if(lu.ne.5.or.lu.ne.6) ntunt = ntunt + 1
                  if(ntunt.eq.mxunit) write(istder, 2005)
                else
                  open(unit=lu,file=fname,form='UNFORMATTED',
     &                 access='SEQUENTIAL', err=100)
                  if(lu.ne.5.or.lu.ne.6) ntunt = ntunt + 1
                  if(ntunt.eq.mxunit) write(istder, 2005)
                end if
C
CMOD            if (vrbose) write (0, 2000) lu, fname(:lnblnk(fname))
                lnblnk = index(fname,' ') -1
                if( lnblnk.lt.0 ) lnblnk = len(fname)
                if (vrbose) write (istder, 2000)
     &             lu, fname(:lnblnk)
C
                fenv = .true.
                go to 200
C
  100           write (istder, 2003) ename(:nb+1)
C:Function "perror" may not exist.
CMOD            call perror (fname(:lnblnk))
                iok = .false.
C
  200       continue
        endif
C
        if (vrbose) then
            if (ienv .and. (.not. fenv))
     &      write (istder, 2001) ename(:nb-1)
C:l-type in format 2004 may be invalid in some systems(SX3).
CMOD        write (istder, 2004) cctl, bzro, apnd
            torf1 = 'false'
            torf2 = 'false'
            torf3 = 'false'
            if(cctl) torf1 = 'true'
            if(bzro) torf2 = 'true'
            if(apnd) torf3 = 'true'
            write (istder, 2004) torf1, torf2, torf3
C:Flushing standard error output has no meaning.
CMOD        call flush (0)
        endif
C
        uioint = iok
C
        return

 2000   format (' uioint: logical unit ', i2,' opened to ', a)
 2001   format (' uioint: no initialization found for ', a)
 2002   format (' uioint: initializing from ', a, 'nn')

C:'$' format specifier may be invalid in some systems.
C:l-type may be invalid in some systems(SX3).
C2003   format ('ioinit: ', a, ' ', $)
C2004   format (' uioint: cctl=', l, ', bzro=', l, ', apnd=', l)
 2003   format (' uioint: ', a, ': FILE OPEN ERROR ')
 2004   format (' uioint: cctl=', a, ', bzro=', a, ', apnd=', a)
 2005   format (' uioint: number of opened unit is upper-limit')
        end
