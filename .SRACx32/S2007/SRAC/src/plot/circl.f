C*************************************************************************
c*    subroutine     circl                                               *
c*                                                                       *
c*    call circl  (xpage,ypage,tho,thf,ro,rf,di)                         *
c*      xpage,ypage  are the coordinates of the starting point of the    *
c*                   arc.                                                *
c*      tho          is the angle for the start of the arc, in degrees.  *
c*      thf          is the angle for the end of the arc, in degrees.    *
c*      ro           is the radius at the start of the arc.              *
c*      rf           is the radius at the end of the arc.                *
c*      di           is a code used to specify the type of line...       *
c*                     0.0  for a solid arc.                             *
c*                     0.5  for a dashed arc.                            *
c*************************************************************************
      SUBROUTINE CIRCL( T, U, TH0, THF, R0, RF, DSHI )
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
      common /gcomxy/ jcomxy(4)
      common /gcomxy/ fct, ycomxy(6)
c
      common    / bldbug /  istep
c
      character*8    com2, com3
      data  com2, com3
     *   / 'arc', 'arnc' /
c
c
      i5 = 4.51 + dshi
      i2 = 2
      x  = t
      y  = u
c
c
      if ( r0 .eq. rf )    then
         if ( psplot .eq. 0 )    then
            write(ipunit,'(a)') 'pifdict begin'
            write(ipunit,'(a)') 'gsave'
            write(ipunit,'(a)') 'stroke'
            write(ipunit,'(a)') 'grestore'
            write(ipunit,'(a)') 'newpath'
            if ( ifplt3 .eq. 0 )    then
               ixpixm  =  psxorg * xpixmm
               iypixm  =  psyorg * ypixmm
               write(ipunit,'(2i5,1x,a)')  ixpixm, iypixm, 'M'
            end if
            psplot  =  1
         end if
         if ( dshi .ne. 0.0 )    then
            write(ipunit,'(a)') 'gsave'
            write(ipunit,'(a)') 'stroke'
            write(ipunit,'(a)') 'grestore'
            write(ipunit,'(a)') 'newpath'
            write(ipunit,'(a)')  '[5 5] 0 setdash'
         end if
         t0   = th0/57.2958
         tf   = thf/57.2958
         x0   = t - r0 * cos(t0)
         y0   = u - r0 * sin(t0)
         xx   = t * fval
         yy   = u * fval
         ixx  = (psxorg + xx) * xpixmm
         iyy  = (psyorg + yy) * ypixmm
         xx   = x0* fval
         yy   = y0* fval
         ix0  = (psxorg + xx) * xpixmm
         iy0  = (psyorg + yy) * ypixmm
         irad = r0  * fval * xpixmm
         ith0 = th0
         ithf = thf
         write(ipunit,'(2i5,1x,a1)') ixx, iyy, 'M'
         if ( th0 .lt. thf )    then
            write(ipunit,'(5i5,1x,a3)')
     *            ix0, iy0, irad, ith0, ithf, com2
         else
            write(ipunit,'(5i5,1x,a4)')
     *            ix0, iy0, irad, ith0, ithf, com3
         end if
         if ( dshi .ne. 0.0 )    then
            write(ipunit,'(a)') 'gsave'
            write(ipunit,'(a)') 'stroke'
            write(ipunit,'(a)') 'grestore'
            write(ipunit,'(a)') 'newpath'
            write(ipunit,'(a)')  '[] 0 setdash'
         end if
         return
      end if
c
c
      call plotgm( x, y, 3 )
      call where( dth, dth, fctr )
c
      knt = 7.0
      r0rrf = abs(r0) + abs(rf) + 0.00001
      dth = 0.8/fct/(r0rrf)/fctr
      t0 = th0/57.2958
      tf = thf/57.2958
      c = x - r0 * cos(t0)
      tn = ( tf - t0 )/dth
c
      if( tf - t0 .lt. 0.0 ) then
         tn = abs(tn)
         dth = -dth
      endif
c
      b = y - r0*sin(t0)
      n = tn
c
      if( n .gt. 0.0 ) then
         tn = ( rf - r0 )/tn
         rn = r0 - tn
         do 4000 i = 1, n
            t00 = t0 + dth*float(i)
            rnn = rn + tn*float(i)
            x = rnn*cos(t00) + c
            y = rnn*sin(t00) + b
c
            if( knt .le. 0 ) then
               i2 = i5 - i2
               knt = 7.0
            endif
            knt = knt - 1
            call plotgm( x, y, i2 )
 4000    continue
      endif
      x = rf*cos(tf) + c
      y = rf*sin(tf)+b
      call plotgm( x, y, i2 )
c
      return
      END
