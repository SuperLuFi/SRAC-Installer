      SUBROUTINE    NEWPEN ( IPEN )
c
c/******************************************************************
c *                                                                *
c *    piflib.h : Header File for Plotter Routine on WS            *
c *                                                                *
c ******************************************************************/
c
c    float xorg, yorg           /* x, y origin on X-Window           */
c    float psxorg, psyorg       /* x, y origin on Post Script        */
c    float xpt0, ypt0           /* Terminate Points of before stroke */
c    float psstrx, psstry       /* Terminate Points of PS stroke     */
c    float xpixmm, ypixmm       /* Value a pixel per mm              */
c    float fval                 /* factor value                      */
c    float xstr, ystr           /* Current Point of where routine    */
c    
c    char iflag                 /* Condition flag for GDFile         */
c    char ifplt3                /* if call plot(,,(+or-)3)           */ 
c    char psplot                /* if drawing before in Post Script  */
c    char ifdraw                /* if drawing before in X-Window     */
c    long ltype                 /* Current Line Type                 */
c    float pscale               /* scaling factor of xpif            */
c    char ifxpif                /* if called from  xpif              */
c    long ipunit                /* ps file logical unit number       */
c    unsigned long color[8]     /* Color value                       */
c    
      common / biflib /
     1    xorg    , yorg    , psxorg  , psyorg  , xpt0    , ypt0    ,
     2    psstrx  , psstry  , xpixmm  , ypixmm  , fval    ,
     3    xstr    , ystr    , iflag   , ifplt3  , psplot  ,
     4    ifdraw  , ltype   , pscale  , ifxpif  , ipunit  , color(8)
c
c/******************************************************************
c *                                                                *
c *    ggsx.h : Header File for GGS Routines on EWS                *
c *                                                                *
c ******************************************************************/
c
      common /gcomxy/ jfilm, kpen, jopt, kerr
      common /gcomxy/ fct, hfct, olx, oly, xrg, yrg, ff
c
      common /ltpcom/ ltp001, ltp002
c
c
      character*16  comm
c
      if ( psplot .eq. 0 )    then
         write(ipunit,'(a)') 'pifdict begin'
         write(ipunit,'(a)') 'gsave'
         write(ipunit,'(a)') 'newpath'
         write(ipunit,'(a)') 'grestore'
         write(ipunit,'(a)') '0 839 translate'
         write(ipunit,'(a)') '-90 rotate'
CKSK     change scale factor only for SRAC95 (plot.f, newpen.f)
CKSK     write(ipunit,'(a)') '.25 .25 scale'
         write(ipunit,'(a)') '.20 .20 scale'
         if ( ifplt3 .eq. 0 )    then
            ipxm    =  psxorg * xpixmm
            ipym    =  psyorg * ypixmm
            comm    =  'M'
            write(ipunit,'(2i5,1x,a1)')  ipxm, ipym, comm
         end if
      end if
c
      write(ipunit,'(a)') 'stroke'
      comm    = 'setlinewidth'
      write(ipunit,'(i3,1x,a12)') ipen*2-1, comm
      ipsx    =  psstrx
      ipsy    =  psstry
      comm    = 'M'
      write(ipunit,'(2i5,1x,a1)')  ipsx, ipsy, comm
      psplot  =  1
c
      return
      END
