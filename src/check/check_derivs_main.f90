! THIS VERSION: SIFDECODE 1.2 - 01/01/2017 AT 13:45 GMT.

!-*-*-*-*-*-  S I F D E C O D E   C H E C K _ D E R I V S _ M A I N  -*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   released with SIFDECODE Version 1.2. January 1st 2017

     PROGRAM CHECK_DERIVS_MAIN
     USE SIFDECODE_random_double
     USE SIFDECODE_check_derivs_double
     IMPLICIT NONE

!  Problem input characteristics

     INTEGER, PARAMETER :: input = 55
     CHARACTER ( LEN = 16 ) :: prbdat = 'OUTSDIF.d'

!  Set precision

     INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

!  Default values for parameters

     INTEGER :: dfiledevice = 26
!    LOGICAL :: write_problem_data = .FALSE.
     LOGICAL :: write_problem_data = .TRUE.
     CHARACTER ( LEN = 30 ) :: dfilename = 'SIF.data'
     LOGICAL :: testal = .FALSE.
     LOGICAL :: dechk  = .FALSE.
     LOGICAL :: dechke = .TRUE.
     LOGICAL :: dechkg = .TRUE.
     LOGICAL :: not_fatal  = .FALSE.
     LOGICAL :: not_fatale = .FALSE.
     LOGICAL :: not_fatalg = .FALSE.
     INTEGER :: print_level = 1

!  Output file characteristics

     INTEGER :: errout = 6
     CHARACTER ( LEN = 10 ) :: pname

     INTEGER, PARAMETER :: out = 6
     REAL ( KIND = wp ), PARAMETER :: zero = 0.0_wp
     REAL ( KIND = wp ), PARAMETER :: one = 1.0_wp
     REAL ( KIND = wp ), PARAMETER :: point1 = 0.1_wp
     REAL ( KIND = wp ), PARAMETER :: epsmch = EPSILON( one )

!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------

     INTEGER :: ntotel, nvrels, nnza, ngpvlu, nepvlu, ncalcf, ncalcg, status
     INTEGER :: neltyp, ngrtyp, ialgor, ieltyp, lfuval, nin, ninmax, nelmax
     INTEGER :: i, j, igrtyp, iauto, iores, alloc_status
     REAL ( KIND = wp ) :: rand
     REAL ( KIND = wp ), DIMENSION( 2 ) :: OBFBND
     LOGICAL :: filexx
     LOGICAL :: second = .TRUE.
!    LOGICAL :: square

     CHARACTER ( LEN = 80 ) :: bad_alloc

!----------------------------
!   D e r i v e d   T y p e s
!----------------------------

     TYPE ( problem_type ) :: prob
     TYPE ( DRCHE_save_type ) :: DRCHE_S
     TYPE ( DRCHG_save_type ) :: DRCHG_S
     TYPE ( RAND_seed ) :: seed

!-----------------------------------------------
!   A l l o c a t a b l e   A r r a y s
!-----------------------------------------------

     INTEGER, ALLOCATABLE, DIMENSION( : ) :: IVAR
     INTEGER, ALLOCATABLE, DIMENSION( : ) :: ICALCF
     INTEGER, ALLOCATABLE, DIMENSION( : ) :: ICALCG
     REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : , : ) :: GVALS
     REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: XT
     REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: DGRAD
     REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: Q
     REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: FT
     REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: FUVALS
     CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: ETYPES
     CHARACTER ( LEN = 10 ), ALLOCATABLE, DIMENSION( : ) :: GTYPES

!  locally used arrays

     INTEGER, ALLOCATABLE, DIMENSION( : ) :: ITEST, IELVAR_temp
     REAL ( KIND = wp ), ALLOCATABLE, DIMENSION( : ) :: X_temp

!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------

     INTERFACE

!  Interface block for RANGE

       SUBROUTINE RANGE ( ielemn, transp, W1, W2, nelvar, ninvar, ieltyp,      &
                          lw1, lw2 )
       INTEGER, INTENT( IN ) :: ielemn, nelvar, ninvar, ieltyp, lw1, lw2
       LOGICAL, INTENT( IN ) :: transp
       REAL ( KIND = KIND( 1.0D+0 ) ), INTENT( IN ), DIMENSION ( lw1 ) :: W1
!      REAL ( KIND = KIND( 1.0D+0 ) ), INTENT( OUT ), DIMENSION ( lw2 ) :: W2
       REAL ( KIND = KIND( 1.0D+0 ) ), DIMENSION ( lw2 ) :: W2
       END SUBROUTINE RANGE

!  Interface block for ELFUN

       SUBROUTINE ELFUN ( FUVALS, XVALUE, EPVALU, ncalcf, ITYPEE, ISTAEV,      &
                          IELVAR, INTVAR, ISTADH, ISTEPA, ICALCF, ltypee,      &
                          lstaev, lelvar, lntvar, lstadh, lstepa, lcalcf,      &
                          lfuval, lxvalu, lepvlu, ifflag, ifstat )
       INTEGER, INTENT( IN ) :: ncalcf, ifflag, ltypee, lstaev, lelvar, lntvar
       INTEGER, INTENT( IN ) :: lstadh, lstepa, lcalcf, lfuval, lxvalu, lepvlu
       INTEGER, INTENT( OUT ) :: ifstat
       INTEGER, INTENT( IN ) :: ITYPEE(LTYPEE), ISTAEV(LSTAEV), IELVAR(LELVAR)
       INTEGER, INTENT( IN ) :: INTVAR(LNTVAR), ISTADH(LSTADH), ISTEPA(LSTEPA)
       INTEGER, INTENT( IN ) :: ICALCF(LCALCF)
       REAL ( KIND = KIND( 1.0D+0 ) ), INTENT( IN ) :: XVALUE(LXVALU)
       REAL ( KIND = KIND( 1.0D+0 ) ), INTENT( IN ) :: EPVALU(LEPVLU)
       REAL ( KIND = KIND( 1.0D+0 ) ), INTENT( INOUT ) :: FUVALS(LFUVAL)
       END SUBROUTINE ELFUN

!  Interface block for GROUP

       SUBROUTINE GROUP ( GVALUE, lgvalu, FVALUE, GPVALU, ncalcg,              &
                          ITYPEG, ISTGPA, ICALCG, ltypeg, lstgpa,              &
                          lcalcg, lfvalu, lgpvlu, derivs, igstat )
       INTEGER, INTENT( IN ) :: lgvalu, ncalcg
       INTEGER, INTENT( IN ) :: ltypeg, lstgpa, lcalcg, lfvalu, lgpvlu
       INTEGER, INTENT( OUT ) :: igstat
       LOGICAL, INTENT( IN ) :: derivs
       INTEGER, INTENT( IN ), DIMENSION ( ltypeg ) :: ITYPEG
       INTEGER, INTENT( IN ), DIMENSION ( lstgpa ) :: ISTGPA
       INTEGER, INTENT( IN ), DIMENSION ( lcalcg ) :: ICALCG
       REAL ( KIND = KIND( 1.0D+0 ) ), INTENT( IN ),                           &
                                       DIMENSION ( lfvalu ) :: FVALUE
       REAL ( KIND = KIND( 1.0D+0 ) ), INTENT( IN ),                           &
                                       DIMENSION ( lgpvlu ) :: GPVALU
       REAL ( KIND = KIND( 1.0D+0 ) ), INTENT( INOUT ),                        &
                                       DIMENSION ( lgvalu, 3 ) :: GVALUE
       END SUBROUTINE GROUP
     END INTERFACE

!  ================================
!   O p e n   d a t a   f i l e s
!  ================================

!  Open the data input file

     OPEN( input, FILE = prbdat, FORM = 'FORMATTED', STATUS = 'OLD'  )
     REWIND input

!  ==========================================================================
!   D a t a    f i l e s   o p e n  ;  p a r t i t i o n   w o r k s p a c e
!  ==========================================================================

!  Input the problem dimensions

     READ( input, 1050 ) prob%n, prob%ng, prob%nel, ntotel,                    &
                         nvrels, nnza, ngpvlu, nepvlu, neltyp, ngrtyp
     READ( input, 1060 ) ialgor, pname, iauto

!  Allocate space for the integer arrays

     ALLOCATE( prob%ISTADG( prob%ng  + 1 ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'prob%ISTADG' ; GO TO 800; END IF
     ALLOCATE( prob%ISTGPA( prob%ng  + 1 ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'prob%ISTGPA' ; GO TO 800; END IF
     ALLOCATE( prob%ISTADA( prob%ng  + 1 ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'prob%ISTADA' ; GO TO 800; END IF
     ALLOCATE( prob%ISTAEV( prob%nel + 1 ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'prob%ISTAEV' ; GO TO 800; END IF
     ALLOCATE( prob%ISTEPA( prob%nel + 1 ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'prob%ISTEPA' ; GO TO 800; END IF
     ALLOCATE( prob%ITYPEG( prob%ng  ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'prob%ITYPEG' ; GO TO 800; END IF
     IF ( ialgor > 1 ) THEN
       ALLOCATE( prob%KNDOFG( prob%ng  ), STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN ; bad_alloc = 'KNDOFG'; GO TO 800; END IF
     END IF
     ALLOCATE( prob%ITYPEE( prob%nel ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'prob%ITYPEE' ; GO TO 800; END IF
     ALLOCATE( prob%INTVAR( prob%nel + 1 ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'prob%INTVAR' ; GO TO 800; END IF
     ALLOCATE( prob%IELING( ntotel ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'prob%IELING' ; GO TO 800; END IF
     ALLOCATE( prob%IELVAR( nvrels ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'prob%IELVAR' ; GO TO 800; END IF
     ALLOCATE( prob%ICNA( nnza ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'prob%ICNA' ; GO TO 800; END IF
     ALLOCATE( prob%ISTADH( prob%nel + 1 ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'prob%ISTADH' ; GO TO 800; END IF
     ALLOCATE( IVAR( prob%n ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN ; bad_alloc = 'IVAR' ; GO TO 800; END IF
     ALLOCATE( ICALCG( prob%ng ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN ; bad_alloc = 'ICALCG' ; GO TO 800; END IF
     ALLOCATE( ICALCF( MAX( prob%ng, prob%nel ) ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN ; bad_alloc = 'ICALCF' ; GO TO 800; END IF

!  Allocate space for the real arrays

     ALLOCATE( prob%A( nnza ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN ; bad_alloc = 'prob%A' ; GO TO 800 ; END IF
     ALLOCATE( prob%B( prob%ng ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN ; bad_alloc = 'prob%B' ; GO TO 800 ; END IF
     ALLOCATE( prob%BL( prob%n ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN ; bad_alloc = 'prob%BL' ; GO TO 800 ; END IF
     ALLOCATE( prob%BU( prob%n ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN ; bad_alloc = 'prob%BU' ; GO TO 800 ; END IF
     ALLOCATE( prob%X( prob%n ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN ; bad_alloc = 'prob%X' ; GO TO 800 ; END IF
     IF ( ialgor > 1 ) THEN
       ALLOCATE( prob%Y( prob%ng ), STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN ; bad_alloc = 'prob%Y' ; GO TO 800 ; END IF
     END IF
     ALLOCATE( prob%C( prob%ng ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN ; bad_alloc = 'prob%C' ; GO TO 800 ; END IF
     ALLOCATE( prob%GPVALU( ngpvlu ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'prob%GPVALU' ; GO TO 800; END IF
     ALLOCATE( prob%EPVALU( nepvlu ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'prob%EPVALU' ; GO TO 800; END IF
     ALLOCATE( prob%ESCALE( ntotel ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'prob%ESCALE' ; GO TO 800; END IF
     ALLOCATE( prob%GSCALE( prob%ng ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'prob%GSCALE' ; GO TO 800; END IF
     ALLOCATE( prob%VSCALE( prob%n ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'prob%VSCALE' ; GO TO 800; END IF
     ALLOCATE( XT( prob%n ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN ; bad_alloc = 'XT' ; GO TO 800 ; END IF
     ALLOCATE( DGRAD( prob%n ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN ; bad_alloc = 'DGRAD' ; GO TO 800 ; END IF
     ALLOCATE( Q( prob%n ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN ; bad_alloc = 'Q' ; GO TO 800 ; END IF
     ALLOCATE( FT( prob%ng ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN ; bad_alloc = 'FT' ; GO TO 800 ; END IF
     ALLOCATE( GVALS( prob%ng , 3 ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN ; bad_alloc = 'GVALS' ; GO TO 800 ; END IF

!  Allocate space for the logical arrays

     ALLOCATE( prob%INTREP( prob%nel ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'prob%INTREP' ; GO TO 800; END IF
     ALLOCATE( prob%GXEQX( prob%ng ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'prob%GXEQX' ; GO TO 800 ; END IF

!  Allocate space for the character arrays

     ALLOCATE( prob%GNAMES( prob%ng ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'prob%GNAMES' ; GO TO 800; END IF
     ALLOCATE( prob%VNAMES( prob%n ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN
       bad_alloc = 'prob%VNAMES' ; GO TO 800; END IF
     ALLOCATE( ETYPES( neltyp ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN ; bad_alloc = 'ETYPES' ; GO TO 800; END IF
     ALLOCATE( GTYPES( ngrtyp ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN ; bad_alloc = 'GTYPES' ; GO TO 800; END IF

!  Set up initial data

     IF ( dechk .OR. testal ) THEN ; dechke = .TRUE. ; dechkg = .TRUE. ; END IF
     IF ( not_fatal ) THEN ; not_fatale = .TRUE. ; not_fatalg = .TRUE. ; END IF

!  If required, print out the (raw) problem data

     IF ( write_problem_data ) THEN
       INQUIRE( FILE = dfilename, EXIST = filexx )
       IF ( filexx ) THEN
          OPEN( dfiledevice, FILE = dfilename, FORM = 'FORMATTED',             &
                STATUS = 'OLD', IOSTAT = iores )
       ELSE
          OPEN( dfiledevice, FILE = dfilename, FORM = 'FORMATTED',             &
                STATUS = 'NEW', IOSTAT = iores )
       END IF
       IF ( iores /= 0 ) THEN
         WRITE( out, "( ' IOSTAT = ', I6, ' when opening file ', A9,           &
        & '. Stopping ' )") iores, dfilename
         STOP
       END IF
     END IF

!  Print out problem data. Input the number of variables, groups,
!  elements and the identity of the objective function group

     IF ( ialgor == 2 ) THEN
       READ( input, 1000 ) i
     END IF
     IF ( write_problem_data )                                                 &
       WRITE( dfiledevice, 1100 ) pname, prob%n, prob%ng, prob%nel

!  Input the starting addresses of the elements in each group, of the
!  parameters used for each group and of the nonzeros of the linear
!  element in each group

     READ( input, 1010 ) prob%ISTADG
     IF ( write_problem_data ) WRITE( dfiledevice, 1110 ) 'ISTADG', prob%ISTADG
     READ( input, 1010 ) prob%ISTGPA
     IF ( write_problem_data ) WRITE( dfiledevice, 1110 ) 'ISTGPA ', prob%ISTGPA
     READ( input, 1010 ) prob%ISTADA
     IF ( write_problem_data ) WRITE( dfiledevice, 1110 ) 'ISTADA', prob%ISTADA

!  Input the starting addresses of the variables and parameters in each element

     READ( input, 1010 ) prob%ISTAEV
     IF ( write_problem_data ) WRITE( dfiledevice, 1110 ) 'ISTAEV', prob%ISTAEV
     READ( input, 1010 ) prob%ISTEPA
     IF ( write_problem_data ) WRITE( dfiledevice, 1110 ) 'ISTEPA ', prob%ISTEPA

!  Input the group type of each group

     READ( input, 1010 ) prob%ITYPEG
     IF ( write_problem_data ) WRITE( dfiledevice, 1110 ) 'ITYPEG', prob%ITYPEG
     IF ( ialgor > 1 ) THEN
       READ( input, 1010 ) prob%KNDOFG
       IF ( write_problem_data )                                               &
         WRITE( dfiledevice, 1110 ) 'KNDOFG', prob%KNDOFG
     END IF

!  Input the element type of each element

     READ( input, 1010 ) prob%ITYPEE
     IF ( write_problem_data ) WRITE( dfiledevice, 1110 ) 'ITYPEE', prob%ITYPEE

!  Input the number of internal variables for each element

     READ( input, 1010 ) prob%INTVAR( : prob%nel )
     IF ( write_problem_data )                                                 &
       WRITE( dfiledevice, 1110 ) 'INTVAR', prob%INTVAR( : prob%nel )

!  Determine the required length of FUVALS.

     lfuval = prob%nel + 2 * prob%n
     DO i = 1, prob%nel
       lfuval = lfuval + ( prob%INTVAR( i ) * ( prob%INTVAR( i ) + 3 ) ) / 2
     END DO

!  Allocate FUVALS.

     ALLOCATE( FUVALS( lfuval ), STAT = alloc_status )
     IF ( alloc_status /= 0 ) THEN ; bad_alloc = 'FUVALS' ; GO TO 800; END IF

!  Input the identity of each individual element

     READ( input, 1010 ) prob%IELING
     IF ( write_problem_data ) WRITE( dfiledevice, 1110 ) 'IELING', prob%IELING

!  Input the variables in each group's elements

     READ( input, 1010 ) prob%IELVAR
     IF ( write_problem_data ) WRITE( dfiledevice, 1110 ) 'IELVAR', prob%IELVAR

!  Input the column addresses of the nonzeros in each linear element

     READ( input, 1010 ) prob%ICNA
     IF ( write_problem_data ) WRITE( dfiledevice, 1110 ) 'ICNA  ', prob%ICNA

!  Input the values of the nonzeros in each linear element, the constant term
!  in each group, the lower and upper bounds on the variables and the starting
!  point for the minimization

     READ( input, 1020 ) prob%A
     IF ( write_problem_data ) WRITE( dfiledevice, 1120 ) 'A     ', prob%A
     READ( input, 1020 ) prob%B
     IF ( write_problem_data ) WRITE( dfiledevice, 1120 ) 'B     ', prob%B
     READ( input, 1020 ) prob%BL, prob%C
     IF ( write_problem_data ) WRITE( dfiledevice, 1120 ) 'BL    ', prob%BL
     READ( input, 1020 ) prob%BU, prob%C
     IF ( write_problem_data ) WRITE( dfiledevice, 1120 ) 'BU    ', prob%BU
     READ( input, 1020 ) prob%X
     IF ( write_problem_data ) WRITE( dfiledevice, 1120 ) 'X     ', prob%X
     IF ( ialgor > 1 ) THEN
       READ( input, 1020 ) prob%Y
       IF ( write_problem_data ) WRITE( dfiledevice, 1120 ) 'Y     ', prob%Y
     END IF

!  Input the parameters in each group

     READ( input, 1020 ) prob%GPVALU
     IF ( write_problem_data ) WRITE( dfiledevice, 1120 ) 'GPVALU', prob%GPVALU

!  Input the parameters in each individual element

     READ( input, 1020 ) prob%EPVALU
     IF ( write_problem_data ) WRITE( dfiledevice, 1120 ) 'EPVALU', prob%EPVALU

!  Input the scale factors for the nonlinear elements

     READ( input, 1020 ) prob%ESCALE
     IF ( write_problem_data ) WRITE( dfiledevice, 1120 ) 'ESCALE', prob%ESCALE

!  Input the scale factors for the groups

     READ( input, 1020 ) prob%GSCALE
     IF ( write_problem_data ) WRITE( dfiledevice, 1120 ) 'GSCALE', prob%GSCALE

!  Input the scale factors for the variables

     READ( input, 1020 ) prob%VSCALE
     IF ( write_problem_data ) WRITE( dfiledevice, 1120 ) 'VSCALE', prob%VSCALE

!  Input the lower and upper bounds on the objective function

     READ( input, 1080 ) OBFBND( 1 ), OBFBND( 2 )
     IF ( write_problem_data )                                                 &
       WRITE( dfiledevice, 1180 ) 'OBFBND', OBFBND( 1 ), OBFBND( 2 )

!  Input a logical array which says whether an element has internal variables

     READ( input, 1030 ) prob%INTREP
     IF ( write_problem_data ) WRITE( dfiledevice, 1130 ) 'INTREP', prob%INTREP

!  Input a logical array which says whether a group is trivial

     READ( input, 1030 ) prob%GXEQX
     IF ( write_problem_data )                                                 &
       WRITE( dfiledevice, 1130 ) 'GXEQX ', prob%GXEQX

!  Input the names given to the groups and to the variables

     READ( input, 1040 ) prob%GNAMES
     IF ( write_problem_data ) WRITE( dfiledevice, 1140 ) 'GNAMES', prob%GNAMES
     READ( input, 1040 ) prob%VNAMES
     IF ( write_problem_data ) WRITE( dfiledevice, 1140 ) 'VNAMES', prob%VNAMES

!  Input the names given to the element and group types

     READ( input, 1040 ) ETYPES
     IF ( write_problem_data ) WRITE( dfiledevice, 1140 ) 'ETYPES', ETYPES
     READ( input, 1040 ) GTYPES
     IF ( write_problem_data ) WRITE( dfiledevice, 1140 ) 'GTYPES', GTYPES
     READ( input, 1010 ) ( j, i = 1, prob%n )

     CLOSE( dfiledevice )

     WRITE( out, "( ' ************* Problem ', A, ' *****************' )" )    &
       TRIM( pname )
     IF ( iauto == 1 ) WRITE( out,                                             &
            "( /, ' =+=+= Automatic derivatives - forward mode  =+=+=' )" )
     IF ( iauto == 2 ) WRITE( out,                                             &
            "( /, ' =+=+= Automatic derivatives - backward mode =+=+=' )" )

!  If required, test the derivatives of the element functions

     CALL RAND_initialize( seed )

     IF ( prob%nel > 0 .AND. dechke ) THEN
       status = 0

!  Allocate temporary space for the tests

       ALLOCATE( ITEST( prob%nel ), STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN ; bad_alloc = 'ITEST' ; GO TO 800
       END IF
       ALLOCATE( IELVAR_temp( MAX( prob%nel, nvrels ) ), STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN ; bad_alloc = 'IELVAR_temp' ; GO TO 800
       END IF

!  Check the derivatives of the element functions at the point XT

       DO j = 1, prob%n
         CALL RAND_random_real( seed, .TRUE., rand )
         IF ( one <= prob%BL( j ) ) THEN
           XT( j ) = prob%BL( j ) + point1 * rand *                            &
             MIN( one, prob%BU( j ) - prob%BL( j ) )
         ELSE
           IF ( one >= prob%BU( j ) ) THEN
             XT( j ) = prob%BU( j ) - point1 * rand *                          &
               MIN( one, prob%BU( j ) - prob%BL( j ) )
           ELSE
             XT( j ) = one + point1 * rand * MIN( one, prob%BU( j ) - one )
           END IF
         END IF
       END DO

!  Test all the nonlinear element functions

       IF ( testal ) THEN
         ncalcf = prob%nel
         DO j = 1, ncalcf
           ITEST( j ) = j
         END DO

!  Test one nonlinear element function of each type

       ELSE
         ncalcf = 0
         IELVAR_temp = 0
         DO j = 1, prob%nel
           ieltyp = prob%ITYPEE( j )
           IF ( IELVAR_temp( ieltyp ) == 0 ) THEN
             IF ( print_level > 0 ) WRITE( errout,                             &
               "( /, ' Element number ', I0,' chosen as representative of',    &
              &     ' element type ', A10 )" ) j, ETYPES( ieltyp )
             ncalcf = ncalcf + 1
             ITEST( ncalcf ) = j
             IELVAR_temp( ieltyp ) = 1
           END IF
         END DO
       END IF

!  Allocate real workspace

       ninmax = 0 ; nelmax = 0
       DO j = 1, prob%nel
         nin = prob%INTVAR( j )
         ninmax = MAX( ninmax, nin )
         nelmax = MAX( nelmax, prob%ISTAEV( j + 1 ) - prob%ISTAEV( j ) )
       END DO
       ALLOCATE( X_temp( nelmax ), STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN ; bad_alloc = 'X_temp' ; GO TO 800
       END IF

!  Check the derivatives of the nonlinear element functions
!  --------------------------------------------------------

 100   CONTINUE
       IF ( iauto == 0 .OR. iauto == - 2 .OR. iauto == 1 .OR. iauto == 2 ) THEN
         CALL DRCHE_check_element_derivatives(                                 &
             prob, ICALCF, ncalcf, XT, FUVALS, lfuval, IELVAR_temp,            &
             X_temp, nelmax, ninmax, epsmch, second, ITEST,                    &
             print_level, out,                                                 &
             RANGE , status, DRCHE_S, ELFUN  = ELFUN  )
       ELSE
         CALL DRCHE_check_element_derivatives(                                 &
             prob, ICALCF, ncalcf, XT, FUVALS, lfuval, IELVAR_temp,            &
             X_temp, nelmax, ninmax, epsmch, second, ITEST,                    &
             print_level, out,                                                 &
             RANGE , status, DRCHE_S )
       END IF

!  compute group values and derivatives as required

       IF ( status == - 1 ) THEN
         CALL ELFUN ( FUVALS, XT, prob%EPVALU, ncalcf, prob%ITYPEE,            &
                      prob%ISTAEV, prob%IELVAR, prob%INTVAR, prob%ISTADH,      &
                      prob%ISTEPA, ICALCF, prob%nel, prob%nel + 1,             &
                      prob%ISTAEV( prob%nel + 1 ) - 1, prob%nel + 1,           &
                      prob%nel + 1, prob%nel + 1, prob%nel, lfuval, prob%n,    &
                      prob%ISTEPA( prob%nel + 1 ) - 1, 1, i )
         j = 2
         IF ( second ) j = 3
         CALL ELFUN ( FUVALS, XT, prob%EPVALU, ncalcf, prob%ITYPEE,            &
                      prob%ISTAEV, prob%IELVAR, prob%INTVAR, prob%ISTADH,      &
                      prob%ISTEPA, ICALCF, prob%nel, prob%nel + 1,             &
                      prob%ISTAEV( prob%nel + 1 ) - 1, prob%nel + 1,           &
                      prob%nel + 1, prob%nel + 1, prob%nel, lfuval, prob%n,    &
                      prob%ISTEPA( prob%nel + 1 ) - 1, j, i )
       END IF
       IF ( status == - 2 )                                                    &
         CALL ELFUN ( FUVALS, X_temp, prob%EPVALU, ncalcf, prob%ITYPEE,        &
                      prob%ISTAEV, IELVAR_temp, prob%INTVAR, prob%ISTADH,      &
                      prob%ISTEPA, ICALCF, prob%nel, prob%nel + 1,             &
                      prob%ISTAEV( prob%nel + 1 ) - 1, prob%nel + 1,           &
                      prob%nel + 1, prob%nel + 1, prob%nel, lfuval, prob%n,    &
                      prob%ISTEPA( prob%nel + 1 ) - 1, 1, i )
       IF ( status == - 3 )                                                    &
         CALL ELFUN ( FUVALS, X_temp, prob%EPVALU, ncalcf, prob%ITYPEE,        &
                      prob%ISTAEV, IELVAR_temp, prob%INTVAR, prob%ISTADH,      &
                      prob%ISTEPA, ICALCF, prob%nel, prob%nel + 1,             &
                      prob%ISTAEV( prob%nel + 1 ) - 1, prob%nel + 1,           &
                      prob%nel + 1, prob%nel + 1, prob%nel, lfuval, prob%n,    &
                      prob%ISTEPA( prob%nel + 1 ) - 1, 2, i )
       IF ( status < 0 ) GO TO 100

       DEALLOCATE( ITEST, IELVAR_temp, X_temp )

!  Stop if there were any warning messages

       IF ( status > 0 .AND. .NOT. not_fatale ) THEN
         IF ( out > 0 .AND. print_level == 0 ) WRITE( out,                     &
           "( /, ' Possible error in element derivative. Stopping ' )" )
         GO TO 990
       END IF
     END IF

!  If required, test the derivatives of the group functions

     IF ( dechkg .AND. prob%ng > 0 ) THEN
       status = 0

!  Allocate temporary space for the tests

       ALLOCATE( ITEST( prob%ng ), STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN ; bad_alloc = 'ITEST' ; GO TO 800
       END IF
       ALLOCATE( IELVAR_temp( prob%ng ), STAT = alloc_status )
       IF ( alloc_status /= 0 ) THEN ; bad_alloc = 'IELVAR_temp' ; GO TO 800
       END IF

!  Check the derivatives of the group functions at the points FT.
!  Test all the nontrivial group functions

       IF ( testal ) THEN
         ncalcg = 0
         DO j = 1, prob%ng
           IF ( prob%ITYPEG( j ) <= 0 ) CYCLE
           ncalcg = ncalcg + 1
           IELVAR_temp( j ) = 1
           ITEST( ncalcg ) = j
           CALL RAND_random_real( seed, .TRUE., rand )
           FT( j ) = rand + point1
         END DO

!  Test one nontrivial group function of each type

       ELSE
         ncalcg = 0
         IELVAR_temp = 0
         DO j = 1, prob%ng
           igrtyp = prob%ITYPEG( j )
           IF ( igrtyp <= 0 ) CYCLE
           IF ( IELVAR_temp( igrtyp ) == 0 ) THEN
             IELVAR_temp( igrtyp ) = 1
             ncalcg = ncalcg + 1
             ITEST( ncalcg ) = j
             IF ( print_level > 0 ) WRITE( errout,                             &
               "( /, ' Group number ', I0, ' chosen as representative of ',    &
              &      'group type ', A10 )" ) j, GTYPES( igrtyp )
             CALL RAND_random_real( seed, .TRUE., rand )
             FT( j ) = rand + point1
           END IF
         END DO
       END IF
       DEALLOCATE( IELVAR_temp )

!  Check the derivatives of the group functions
!  --------------------------------------------

  200  CONTINUE

       IF ( iauto == 0 .OR. iauto == - 3 ) THEN
         CALL DRCHG_check_group_derivatives(                                   &
             prob, FT, GVALS, ITEST, ncalcg, epsmch,                           &
             print_level, out, status, DRCHG_S, GROUP = GROUP )
       ELSE IF ( iauto == 1 .OR. iauto == 2 ) THEN
         CALL DRCHG_check_group_derivatives(                                   &
             prob, FT, GVALS, ITEST, ncalcg, epsmch,                           &
             print_level, out, status, DRCHG_S, GROUP = GROUP )
       ELSE
         CALL DRCHG_check_group_derivatives(                                   &
             prob, FT, GVALS, ITEST, ncalcg, epsmch,                           &
             print_level, out, status, DRCHG_S )
       END IF

!  compute group values and derivatives as required

       IF ( status == - 1 .OR. status == - 2 )                                 &
         CALL GROUP ( GVALS , prob%ng, FT    , prob%GPVALU,                    &
                      ncalcg, prob%ITYPEG, prob%ISTGPA, ITEST,                 &
                      prob%ng, prob%ng + 1, prob%ng, prob%ng,                  &
                      prob%ISTGPA( prob%ng + 1 ) - 1, .FALSE., i )
       IF ( status == - 1 .OR. status == - 3 )                                 &
         CALL GROUP ( GVALS , prob%ng, FT    , prob%GPVALU,                    &
                      ncalcg, prob%ITYPEG, prob%ISTGPA, ITEST,                 &
                      prob%ng, prob%ng + 1, prob%ng, prob%ng,                  &
                      prob%ISTGPA( prob%ng + 1 ) - 1, .TRUE. , i )
       IF ( status < 0 ) GO TO 200
       DEALLOCATE( ITEST )

!  Stop if there were any warning messages

       IF ( status > 0 .AND. .NOT. not_fatalg ) THEN
         IF ( out > 0 .AND. print_level == 0 ) WRITE( out,                     &
           "( /, ' Possible error in group derivative. Stopping ' )" )
         GO TO 990
       END IF
     END IF
     GO TO 900

!  Allocation errors

 800 CONTINUE
     WRITE( out, "( ' ** Allocation error (status = ', I0, ') for ', A )" )    &
       alloc_status, TRIM( bad_alloc )

!  End of execution

 900 CONTINUE

! De-allocate all arrays

     DEALLOCATE( prob%ISTADG, prob%ISTGPA, prob%ISTADA, STAT = alloc_status )
     DEALLOCATE( prob%GXEQX , prob%IELING, prob%ISTAEV, STAT = alloc_status )
     DEALLOCATE( prob%ISTEPA, prob%ITYPEG, GTYPES, STAT = alloc_status )
     DEALLOCATE( prob%ITYPEE, prob%IELVAR, prob%ICNA, STAT = alloc_status )
     DEALLOCATE( prob%ISTADH, prob%INTVAR, IVAR, STAT = alloc_status )
     DEALLOCATE( ICALCG, ICALCF, prob%A, prob%B, prob%X, STAT = alloc_status )
     DEALLOCATE( prob%GPVALU, prob%EPVALU, prob%ESCALE, STAT = alloc_status )
     DEALLOCATE( prob%GSCALE, prob%VSCALE, XT, DGRAD , STAT = alloc_status )
     DEALLOCATE( Q, FT, GVALS , prob%BL, prob%BU, ETYPES, STAT = alloc_status )
     DEALLOCATE( prob%INTREP, prob%GNAMES, prob%VNAMES, STAT = alloc_status )
     DEALLOCATE( prob%C, STAT = alloc_status )
     IF ( ialgor > 1 )                                                         &
       DEALLOCATE( prob%Y, prob%KNDOFG, STAT = alloc_status )

 990 CONTINUE

!  Close the data input file

     CLOSE( input  )
     STOP

!  Non-executable statements

 1000  FORMAT( I10 )
 1010  FORMAT( ( 10I8 ) )
 1020  FORMAT( ( 1P, 4D16.8 ) )
 1030  FORMAT( ( 72L1 ) )
 1040  FORMAT( ( 8A10 ) )
 1050  FORMAT( 10I10 )
 1060  FORMAT( I2, A10, I2 )
 1080  FORMAT( 1P, 2D16.8 )
 1100  FORMAT( A10, 3I8 )
 1110  FORMAT( 1X, A6, /, ( 1X, 12I6 ) )
 1120  FORMAT( 1X, A6, /, ( 1X, 1P, 4D16.8 ) )
 1130  FORMAT( 1X, A6, /, ( 1X, 72L1 ) )
 1140  FORMAT( 1X, A6, /, ( 1X, 8A10 ) )
 1180  FORMAT( 1X, A6, /, 1P, 2D16.6 )

   END PROGRAM CHECK_DERIVS_MAIN
