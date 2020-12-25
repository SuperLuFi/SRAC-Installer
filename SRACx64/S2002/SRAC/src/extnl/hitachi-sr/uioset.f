c=====================================================================
c  call uioint for initialization of i/o unit 
c  the uioint is originally "ioinit.f" in sun-ews
c  set system dependent parameter 
c
c        cctl        is .true. to turn on fortran-66 carriage control
c        bzro        is .true. to cause blank space to be zero on input
c        apnd        is .true. to open files at their end
c        prefix      is a string defining environment variables to
c                       be used to initialize logical units.
c                       ex. setenv fu02 file2.dat
c        vrbose      is .true. if the caller wants output showing the
c                       lu association
c        istder      is a device number of standerd error output
c        ideft       =0 : not open for the device prefix is not defined.
c                    =1 : open by defalt name(temp.nn) if prefix is not
c                         defined and ioform .GE. 0 (except 5,6 istder).
c        mxunit      maximum number of openable i/o devices
c                    mxunit = 64(Sun), 48(HP)
c
c=====================================================================
      SUBROUTINE UIOSET
      COMMON /USRSYS/ IBIT64, ISTDER, MXUNIT, IDEFT
c
      character prefix*8
      logical   cctl, bzro, apnd, vrbose, ret, uioint
c
      cctl   = .true.
      bzro   = .false. 
      apnd   = .false. 
      vrbose = .false. 
c     vrbose = .true. 
      prefix = 'fu'
c     istder, ideft, mxunit are given by usystm.f
c
c     systems other than BSD UNIX : ex. HP(istder = 7)
c     systems for that rewind is allowed only for already opened device,
c     set ideft = 1 (ex. HP)
c     in HP, maximum number of openable i/o devices is 48.
c 
      ret = uioint(cctl,bzro,apnd,prefix,vrbose,istder,ideft,mxunit)
      if (ret) then
       return
      else
       write(6,*) ' I/O INITIALIZATION ERROR, STOP AT UIOSET ROUTINE'
       stop
      end if
      end
