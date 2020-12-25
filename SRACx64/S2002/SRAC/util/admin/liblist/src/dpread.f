      SUBROUTINE DPREAD(IDENT,DIRNAM,ISWFT,NTFT,IIRES,IOUT,IPRN)
C
      COMMON  /PARAM/  NTEMP,NSIG,TEMP(4),SIG0(8),NGMAX,NGMIN,IFS,
     +           IFISS,LD(4),LA(4),LTH(4),IOPT(3),IFREAC(6),AMASS
      COMMON  /WORK /  A(20000)
      DIMENSION        IA(20000)
      EQUIVALENCE      (A(1),IA(1))
      CHARACTER*4      IDENT(2)
      CHARACTER        DIRNAM*120,MEMBER*8
C
C  ** SRAT OF PROCESS
C
      CALL ICLEA(100, IA    ,  0)
C
      MEMBER = IDENT(1)//'0000'
      MEMBER(1:1) = 'C'
      LENG = 0
      CALL PDSIN(DIRNAM,MEMBER,IA,LENG,IOUT,IPRN,IRC)
      CALL PDSER('DPREAD',DIRNAM,MEMBER,IRC,IOUT)
C
      ICAPT  = IA(1)
      IFISS  = IA(2)
      IRES   = IA(3)
      LTOT   = IA(4)
      DO 101 I = 1,4
      LTH(I)   = IA(4+I)
      LA(I)    = IA(8+I)
      LD(I)    = IA(12+I)
      TEMP(I)  = A(29+I)
  101 CONTINUE
      ISWFT  = IA(17)
      IFTR   = IA(18)
      IFC    = IA(19)
      IFF    = IA(20)
      IFE    = IA(21)
      IFER   = IA(22)
      NGMIN  = IA(23)
      NGMAX  = IA(24)
      NSIG   = IA(25)
      NTFT   = IA(26)
      IIRES  = IRES
      RETURN
      END
