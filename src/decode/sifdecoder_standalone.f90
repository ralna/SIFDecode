! THIS VERSION: SIFDECODE 2.6 - 2025-04-21 AT 08:55 GMT.

!-*-*-*-*-*-  S I F D E C O D E R _ S T A N A L O N E _ M O D U L E  -*-*-*-*-*-

!  Copyright reserved, Fowkes/Gould/Montoison/Orban, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   Original version SIFDECODE 2.4, August 11th, 2024

!  For full documentation, see
!   http://galahad.rl.ac.uk/galahad-www/specs.html

  MODULE SIFDECODER_standalone

    USE SIFDECODE
    IMPLICIT NONE

    PRIVATE
    PUBLIC :: SIFDECODER_decode

    INTEGER, PARAMETER :: len_var = 10
    INTEGER, PARAMETER :: len_val = 12
    INTEGER, PARAMETER :: in_sif_unit = 61
    INTEGER, PARAMETER :: out_sif_unit = 62
    LOGICAL, PARAMETER :: debug = .FALSE.
    LOGICAL, PARAMETER :: noname = .FALSE.

!  assign the remaining i/o unit numbers

    INTEGER, PARAMETER :: outda = 55
    INTEGER, PARAMETER :: outfn = 52
    INTEGER, PARAMETER :: outra = 53
    INTEGER, PARAMETER :: outgr = 54
    INTEGER, PARAMETER :: outex = 57
    INTEGER, PARAMETER :: outfd = 59
    INTEGER, PARAMETER :: outea = 66
    INTEGER, PARAMETER :: outgd = 63
    INTEGER, PARAMETER :: outem = 67
    INTEGER, PARAMETER :: outff = 0
    INTEGER, PARAMETER :: outgf = 0

    INTEGER, PARAMETER :: out_old = 71
    INTEGER, PARAMETER :: out_new = 72

!  - - - - - - - - - -
!   data derived type
!  - - - - - - - - - -

    TYPE, PUBLIC :: SIFDECODER_param_type
      PRIVATE
      INTEGER :: n_params
      INTEGER, ALLOCATABLE, DIMENSION( : ) :: first, match, last
      CHARACTER ( LEN = len_var ), ALLOCATABLE, DIMENSION( : ) :: var
      CHARACTER ( LEN = len_val ), ALLOCATABLE, DIMENSION( : ) :: val
    END TYPE SIFDECODER_param_type

  CONTAINS

!-*-*-*-*-  S I F D E C O D E R _ D E C O D E _ S U B R O U T I N E  -*-*-*-*-*-

    SUBROUTINE SIFDECODER_decode( name, status, out, size, realpr, start,      &
                                  auto, package, output_level, deriv_check,    &
                                  add_suffix, show_params, param_list, force )

!  decode a given SIF file to produce static data (OUTSDIF.d) and 
!  evaluation routines (ELFUN.f, GROUP.f, RANGE.f, EXTER.f or similar names) 

! ----------------
! dummy arguments:
! ----------------

! name: name of sif file

      CHARACTER ( LEN = * ), INTENT( IN ) :: name

! status: return status (0=success)

      INTEGER, INTENT( OUT ) :: status

! out: unit number for any messages (<6=no output, default = 6)

      INTEGER, OPTIONAL :: out

! size: specify rough size or problems (0=debug, 1=small, 2=medium, 
!       3=large(default))

      INTEGER, OPTIONAL :: size

!  realpr: number of bits for output reals (32, 64, 128, defaut = 64) 

      INTEGER, OPTIONAL :: realpr 

! start: provided start point from SIF file used (1-3, default = 1) 

      INTEGER, OPTIONAL :: start

! auto: 1 for forward automatic differentiation, 2 for backward, default=0

      INTEGER, OPTIONAL :: auto

! package: intended calling package, 1=lancelot, 2=baria, 3=cutest (default)

      INTEGER, OPTIONAL :: package

! output_level: 0=none(default),-ve verbose, >0 debug

      INTEGER, OPTIONAL :: output_level

! deriv_check: deriv_check derivatives if 1 (default = 0)

      INTEGER, OPTIONAL :: deriv_check

! add_suffix: add the suffix _problem to all generated filenames if 1 
!             (default 0)
 
      INTEGER, OPTIONAL :: add_suffix

! show_params: display parameters if 1 and stop (default 0)
 
      INTEGER, OPTIONAL :: show_params

! param_list: list (comma separated) non-default parameter values that the 
!             specified  SIF file will be cast against. If a setting is not 
!             explicit in the SIF file, no action is taken unless force is 1

      CHARACTER ( LEN = * ), OPTIONAL :: param_list

! force: 1 = force the setting of the parameters named in param_list to take
!        the given values, even if those values are not predefined in the
!        SIF file (default, don't force = 0)

      INTEGER, OPTIONAL :: force

! -----------------
!  local variables:
! -----------------

      INTEGER :: out_used, size_used, realpr_used, start_used, package_used
      INTEGER :: auto_used, output_level_used, deriv_check_used
      INTEGER :: add_suffix_used, show_params_used, force_used, file_size
      INTEGER :: i, j, len_params, in_sif, out_sif, infn, ingr, inex
      LOGICAL :: var, exists
      TYPE( SIFDECODER_param_type ) :: params
      CHARACTER ( LEN = LEN_TRIM( name ) ) sifname

!  i/o file names

      CHARACTER ( LEN = 24 ) :: prbout = REPEAT( ' ', 24 )
      CHARACTER ( LEN = 24 ) :: prbfn = REPEAT( ' ', 24 )
      CHARACTER ( LEN = 24 ) :: prbff = REPEAT( ' ', 24 )
      CHARACTER ( LEN = 24 ) :: prbfd = REPEAT( ' ', 24 )
      CHARACTER ( LEN = 24 ) :: prbra = REPEAT( ' ', 24 )
      CHARACTER ( LEN = 24 ) :: prbgr = REPEAT( ' ', 24 )
      CHARACTER ( LEN = 24 ) :: prbgf = REPEAT( ' ', 24 )
      CHARACTER ( LEN = 24 ) :: prbgd = REPEAT( ' ', 24 )
      CHARACTER ( LEN = 24 ) :: prbet = REPEAT( ' ', 24 )
      CHARACTER ( LEN = 24 ) :: prbex = REPEAT( ' ', 24 )
      CHARACTER ( LEN = 24 ) :: prbea = REPEAT( ' ', 24 )

!  deriv_check for dummy arguments and set defaults (see dummy arguments)

      IF ( PRESENT( out ) ) THEN
        out_used = out
      ELSE
        out_used = 6
      END IF

      IF ( PRESENT( size ) ) THEN
        size_used = size
      ELSE
        size_used = 3
      END IF

      IF ( PRESENT( start ) ) THEN
        start_used = start
      ELSE
        start_used = 1
      END IF

      IF ( PRESENT( realpr ) ) THEN
        realpr_used = realpr
      ELSE
        realpr_used = 64
      END IF

      IF ( PRESENT( package ) ) THEN
        package_used = package
      ELSE
        package_used = 3
      END IF

      IF ( PRESENT( output_level ) ) THEN
        output_level_used = output_level
      ELSE
        output_level_used = 0
      END IF

      IF ( PRESENT( deriv_check ) ) THEN
        deriv_check_used = deriv_check
      ELSE
        deriv_check_used = 0
      END IF

      IF ( PRESENT( add_suffix ) ) THEN
        add_suffix_used = add_suffix
      ELSE
        add_suffix_used = 0
      END IF

      IF ( PRESENT( show_params ) ) THEN
        show_params_used = show_params
      ELSE
        show_params_used = 0
      END IF

      IF ( PRESENT( auto ) ) THEN
        auto_used = auto
      ELSE
        auto_used = 0
      END IF

      IF ( PRESENT( force ) ) THEN
        force_used = force
      ELSE
        force_used = 0
      END IF

!  open the SIF file

      in_sif = in_sif_unit
      OPEN( in_sif, FILE = name, FORM = 'FORMATTED', STATUS = 'UNKNOWN' )
      REWIND in_sif

!  if required, list the parameters and exit

      IF (  show_params_used == 1 ) THEN
        CALL SIFDECODER_list_parameters( in_sif, 6, status )
        RETURN
      END IF

!  pull the param string apart to separate each parameter and its desired value

      IF ( PRESENT( param_list ) ) THEN
        len_params = LEN( TRIM( param_list ) )
        IF ( len_params > 0 ) THEN
          params%n_params = 0
          DO i = 1, len_params
            IF ( param_list( i : i ) == '=' )                                  &
              params%n_params = params%n_params + 1
          END DO

          ALLOCATE( params%var( params%n_params ),                             &
                    params%val( params%n_params ),                             &
                    params%first( params%n_params ),                           &
                    params%match( params%n_params ),                           &
                    params%last( params%n_params ), stat = status )
          IF ( status /= 0 ) THEN ; status = 1 ; RETURN ; END IF

          params%n_params = 1
          var = .TRUE. ; j = 0
          DO i = 1, len_params
            IF ( var .AND. j == 0 ) THEN
              params%var( params%n_params ) = REPEAT( ' ', len_var )
              params%val( params%n_params ) = REPEAT( ' ', len_val )
            END IF
            IF ( param_list( i : i ) == '=' ) THEN
              var = .FALSE. ; j = 0
            ELSE IF ( param_list( i : i ) == ',' ) THEN
              var = .TRUE. ; j = 0 ; params%n_params = params%n_params + 1
            ELSE
              j = j + 1
              IF ( var ) THEN
                params%var( params%n_params )( j : j ) = param_list( i : i )
              ELSE
                params%val( params%n_params )( j : j ) = param_list( i : i )
              END IF
            END IF
          END DO
        ELSE
          params%n_params = 0
        END IF
        IF ( debug ) THEN
          DO i = 1, params%n_params
            WRITE( 6, "( ' param ', i0, ' var = ', A, ' val = ', A )" ) &
             i, TRIM( params%var( i ) ), TRIM( params%val( i ) )
          END DO
        END IF
      ELSE
        params%n_params = 0
      END IF

!  preproccess the SIF file if necessary to set parameter values as required

      IF ( params%n_params > 0 ) THEN
        out_sif = out_sif_unit
        OPEN( out_sif, FORM = 'FORMATTED', STATUS = 'SCRATCH' )
        REWIND out_sif
        CALL SIFDECODER_preprocess( in_sif, out_sif, params, force_used,       &
                                    out_used, status )
        CLOSE( in_sif )
        in_sif = out_sif
      END IF

!  set names for output files, with additional _s in the single precsion case,
!  and _q in the quadruple precision case

      IF ( add_suffix_used == 1 ) THEN
        j = LEN_TRIM( name )
        sifname = name
        IF ( sifname( j - 3 : j ) == '.SIF' ) THEN
          sifname( j - 3 : j ) = REPEAT( ' ', 4 )
          j = j - 4
        END IF
        DO i = j, 1, - 1
          IF ( sifname( i : i ) == '/' .OR. sifname( i : i ) == '\' ) THEN
            sifname( 1 : i ) = REPEAT( ' ', i )
            sifname = ADJUSTL( sifname )
            EXIT
          END IF
        END DO
        IF ( realpr == 32 ) THEN
          prbout = 'OUTSDIF_' // TRIM( sifname ) // '_s.d'
          prbfn  = 'ELFUN_' // TRIM( sifname ) // '_s.f'
          prbff  = 'ELFUNF_' // TRIM( sifname ) // '_s.f'
          prbfd  = 'ELFUND_' // TRIM( sifname ) // '_s.f'
          prbra  = 'RANGE_' // TRIM( sifname ) // '_s.f'
          prbgr  = 'GROUP_' // TRIM( sifname ) // '_s.f'
          prbgf  = 'GROUPF_' // TRIM( sifname ) // '_s.f'
          prbgd  = 'GROUPD_' // TRIM( sifname ) // '_s.f'
          prbet  = 'SETTYP_' // TRIM( sifname ) // '_s.f'
          prbex  = 'EXTER_' // TRIM( sifname ) // '_s.f'
          prbea  = 'EXTERA_' // TRIM( sifname ) // '_s.f'
        ELSE IF ( realpr == 128 ) THEN
          prbout = 'OUTSDIF_' // TRIM( sifname ) // '_q.d'
          prbfn  = 'ELFUN_' // TRIM( sifname ) // '_q.f'
          prbff  = 'ELFUNF_' // TRIM( sifname ) // '_q.f'
          prbfd  = 'ELFUND_' // TRIM( sifname ) // '_q.f'
          prbra  = 'RANGE_' // TRIM( sifname ) // '_q.f'
          prbgr  = 'GROUP_' // TRIM( sifname ) // '_q.f'
          prbgf  = 'GROUPF_' // TRIM( sifname ) // '_q.f'
          prbgd  = 'GROUPD_' // TRIM( sifname ) // '_q.f'
          prbet  = 'SETTYP_' // TRIM( sifname ) // '_q.f'
          prbex  = 'EXTER_' // TRIM( sifname ) // '_q.f'
          prbea  = 'EXTERA_' // TRIM( sifname ) // '_q.f'
        ELSE
          prbout = 'OUTSDIF_' // TRIM( sifname ) // '.d'
          prbfn  = 'ELFUN_' // TRIM( sifname ) // '.f'
          prbff  = 'ELFUNF_' // TRIM( sifname ) // '.f'
          prbfd  = 'ELFUND_' // TRIM( sifname ) // '.f'
          prbra  = 'RANGE_' // TRIM( sifname ) // '.f'
          prbgr  = 'GROUP_' // TRIM( sifname ) // '.f'
          prbgf  = 'GROUPF_' // TRIM( sifname ) // '.f'
          prbgd  = 'GROUPD_' // TRIM( sifname ) // '.f'
          prbet  = 'SETTYP_' // TRIM( sifname ) // '.f'
          prbex  = 'EXTER_' // TRIM( sifname ) // '.f '
          prbea  = 'EXTERA_' // TRIM( sifname ) // '.f'
        END IF
      ELSE
        IF ( realpr == 32 ) THEN
          prbout = 'OUTSDIF.d              '
          prbfn  = 'ELFUN_s.f              '
          prbff  = 'ELFUNF_s.f             '
          prbfd  = 'ELFUND_s.f             '
          prbra  = 'RANGE_s.f              '
          prbgr  = 'GROUP_s.f              '
          prbgf  = 'GROUPF_s.f             '
          prbgd  = 'GROUPD_s.f             '
          prbet  = 'SETTYP_s.f             '
          prbex  = 'EXTER_s.f              '
          prbea  = 'EXTERA_s.f             '
        ELSE IF ( realpr == 128 ) THEN
          prbout = 'OUTSDIF.d              '
          prbfn  = 'ELFUN_q.f              '
          prbff  = 'ELFUNF_q.f             '
          prbfd  = 'ELFUND_q.f             '
          prbra  = 'RANGE_q.f              '
          prbgr  = 'GROUP_q.f              '
          prbgf  = 'GROUPF_q.f             '
          prbgd  = 'GROUPD_q.f             '
          prbet  = 'SETTYP_q.f             '
          prbex  = 'EXTER_q.f              '
          prbea  = 'EXTERA_q.f             '
        ELSE
          prbout = 'OUTSDIF.d              '
          prbfn  = 'ELFUN.f                '
          prbff  = 'ELFUNF.f               '
          prbfd  = 'ELFUND.f               '
          prbra  = 'RANGE.f                '
          prbgr  = 'GROUP.f                '
          prbgf  = 'GROUPF.f               '
          prbgd  = 'GROUPD.f               '
          prbet  = 'SETTYP.f               '
          prbex  = 'EXTER.f                '
          prbea  = 'EXTERA.f               '
        END IF
      END IF

!  open the relevant files - UNIX-like systems

      OPEN( outda, FILE = prbout, FORM = 'FORMATTED', STATUS = 'UNKNOWN' )
      REWIND outda
      OPEN( outra, FILE = prbra,  FORM = 'FORMATTED', STATUS = 'UNKNOWN' )
      REWIND outra
      OPEN( outex, FILE = prbex,  FORM = 'FORMATTED', STATUS = 'UNKNOWN' )
      REWIND outex
      IF ( auto == 0 ) THEN
        OPEN( outfn, FILE = prbfn,  FORM = 'FORMATTED', STATUS = 'UNKNOWN' )
        REWIND OUTFN
        OPEN( outgr, FILE = prbgr,  FORM = 'FORMATTED', STATUS = 'UNKNOWN' )
        REWIND outgr
      ELSE
        IF ( outff > 0 ) THEN
          OPEN( outff, FILE = prbff,  FORM = 'FORMATTED', STATUS = 'UNKNOWN' )
          REWIND outff
        END IF
        OPEN( outfd, FILE = prbfd,  FORM = 'FORMATTED', STATUS = 'UNKNOWN' )
        REWIND outfd
        IF ( outgf > 0 ) THEN
          OPEN( outgf, FILE = prbgf,  FORM = 'FORMATTED', STATUS = 'UNKNOWN' )
          REWIND outgf
        END IF
        OPEN( outgd, FILE = prbgd,  FORM = 'FORMATTED', STATUS = 'UNKNOWN' )
        REWIND outgd
        OPEN( outea, FILE = prbea,  FORM = 'FORMATTED', STATUS = 'UNKNOWN' )
        REWIND outea
      END IF
      OPEN( UNIT = outem )

!  now decode the (possibly preprocessed) SIF file

      infn = in_sif ; ingr = in_sif ; inex = in_sif
      CALL SIFDECODE_decode( in_sif, outda, infn, outfn, outff, outfd, outra,  &
                             ingr, outgr, outgf, outgd, inex, outex, outem,    &
                             outea, output_level_used, out_used, noname,       &
                             package_used, auto_used, 2, realpr_used,          &
                             size_used, start_used, status )

!  close the opened files

      CLOSE( in_sif )
      IF ( status == 0 ) THEN
        WRITE( out, "( ' File successfully decoded' )" )

!  if automatic differentation has been requested, move the generated
!  files to their appropriate destinations

        IF ( auto_used > 0 ) THEN
          INQUIRE( FILE = prbfd, EXIST = exists )
          IF ( exists ) CALL SIFDECODER_move_file( prbfd, prbfn, status )
          INQUIRE( FILE = prbgd, EXIST = exists )
          IF ( exists ) CALL SIFDECODER_move_file( prbgd, prbgr, status )
          INQUIRE( FILE = prbea, EXIST = exists )
          IF ( exists ) CALL SIFDECODER_move_file( prbea, prbex, status )
        END IF

        CLOSE( outda )
        CLOSE( outra )
        INQUIRE( UNIT = outex, SIZE = file_size )
        IF ( file_size > 0 ) THEN
          CLOSE( outex )
        ELSE
          CLOSE( outex, STATUS = 'DELETE' )
        END IF
        IF ( auto == 0 ) THEN
          CLOSE( outfn )
          CLOSE( outgr )
        ELSE
          IF ( outff > 0 ) CLOSE( outff )
          CLOSE( outfd )
          IF ( outgf > 0 ) CLOSE( outgf )
          CLOSE( outgd )
          CLOSE( outea )
        END IF

!  if an error has been discovered, delete the output files

      ELSE
        WRITE( out, "( ' Decoding failure, status = ', I0 )" ) status
        CLOSE( outda, STATUS = 'DELETE' )
        CLOSE( outra, STATUS = 'DELETE' )
        IF ( auto == 0 ) THEN
          CLOSE( outfn, STATUS = 'DELETE' )
          CLOSE( outgr, STATUS = 'DELETE' )
          CLOSE( outex, STATUS = 'DELETE' )
        ELSE
          IF ( outff > 0 ) CLOSE( outff, STATUS = 'DELETE' )
          CLOSE( outfd, STATUS = 'DELETE' )
          IF ( outgf > 0 ) CLOSE( outgf, STATUS = 'DELETE' )
          CLOSE( outgd, STATUS = 'DELETE' )
          CLOSE( outea, STATUS = 'DELETE' )
        END IF
      END IF
      CLOSE( outem, STATUS = 'DELETE' )

      RETURN

!  end of subroutine SIFDECODER_decode

    END SUBROUTINE SIFDECODER_decode

!-*-*-  S I F D E C O D E R _ P R E P R O C E S S _ S U B R O U T I N E  -*-*-*-

    SUBROUTINE SIFDECODER_preprocess( in_sif, out_sif, params, force,          &
                                      out, status )

!  preprocess a SIF file to give parameter values specified values

!  dummy arguments

      INTEGER, INTENT( IN ) :: in_sif, out_sif, out, force
      INTEGER, INTENT( OUT ) :: status
      TYPE( SIFDECODER_param_type ), INTENT( INOUT ) :: params

!  local variables

      INTEGER :: i, l, ll, length
      CHARACTER ( LEN = 72 ) :: line
      CHARACTER ( LEN = len_var ) :: var
      CHARACTER ( LEN = len_val ) :: val

!  first, last and match are the line numbers for the first occurence, 
!  the last occurence and the match for each parameter. Initialize these

      params%first( : params%n_params ) = - 1
      params%last( : params%n_params ) = - 1
      params%match( : params%n_params ) = - 1

!  run through the SIF file, looking for parameter lines

      l = 1
      DO
       line = REPEAT( ' ', 72 )
       READ( UNIT = in_sif, FMT = "( A72 )", END = 100, ERR = 100 ) line
       length = LEN( TRIM( line ) )
       IF ( length >= 50 ) THEN

!  this is a paramter line

         IF ( line( 40 : 50 ) == '$-PARAMETER' ) THEN
           var = ADJUSTL( line( 5 : 14 ) )
           val = ADJUSTL( line( 25 : 36 ) )

!  see if this is on the list of parameters to be specified, and if so
!  if the value is on the "allowed" list that may (optimally) be enfoced

           DO i = 1, params%n_params
             IF ( var == params%var( i ) ) THEN
               IF ( params%first( i ) < 0 ) params%first( i ) = l
               params%last( i ) = l
               IF ( val == params%val( i ) ) params%match( i ) = l
               EXIT
             END IF
           END DO
         END IF
       END IF
       l = l + 1
      END DO

  100 CONTINUE
      IF ( debug ) THEN
        DO i = 1, params%n_params
          WRITE( 6, "( ' param ', i0, ' first, match, last =', 3( 1X, I0 ) )" )&
           i, params%first( i ), params%match( i ), params%last( i )
        END DO
      END IF

!  force specified parameter values

      IF ( force == 0 ) THEN
        i = COUNT( params%match( : params%n_params ) == - 1 )
        IF ( i > 0 ) THEN
          status = - 3
          IF ( out > 5 )                                                       &
            WRITE( out, "( 1X, I0, ' parameters unmatched and not forced' )" ) i
        END IF

!  if enforcement is not required, simply change the value of the parameter
!  on its last mention

      ELSE
        DO i = 1, params%n_params
          IF ( params%match( i ) < 0 ) params%match( i ) = params%last( i )
        END DO
      END IF

!  generate the preproccessed SIF file

      REWIND in_sif
      l = 1
      DO
       line = REPEAT( ' ', 72 )
       READ( UNIT = in_sif, FMT = "( A72 )", END = 200, ERR = 200 ) line
       length = LEN( TRIM( line ) )
       ll = 0
       DO i = 1, params%n_params
         IF ( l >= params%first( i ) .AND. l <= params%last( i ) ) THEN
           IF ( force == 1 .OR. params%match( i ) > 0 ) THEN
             ll = i ; EXIT
           END IF
         END IF
       END DO
       IF ( ll > 0 ) THEN
         IF ( l == params%match( ll ) ) THEN
           line( 1 : 1 ) = ' '
           line( 25 : 36 ) = params%val( ll ) 
         ELSE
!          line( 1 : 1 ) = '*'
           l = l + 1
           CYCLE
         END IF           
       END IF           
       WRITE( out_sif, "( A )" ) TRIM( line )
       l = l + 1
      END DO

  200 CONTINUE
      REWIND out_sif

!  for debugging, print the preprocessed file

      IF ( debug ) THEN
        DO
          line = REPEAT( ' ', 72 )
          READ( UNIT = out_sif, FMT = "( A72 )", END = 300, ERR = 300 ) line
          WRITE( out, "( A )" ) TRIM( line )
        END DO

  300   CONTINUE
        REWIND out_sif
      END IF

      status = 0
      RETURN

!  end of subroutine SIFDECODER_preprocess

    END SUBROUTINE SIFDECODER_preprocess

!-  S I F D E C O D E R _ L I S T _ P A R A M E T E R S _ S U B R O U T I N E  -

    SUBROUTINE SIFDECODER_list_parameters( in_sif, out, status )

!  preprocess a SIF file to give parameter values specified values

!  dummy arguments

      INTEGER, INTENT( IN ) :: in_sif, out
      INTEGER, INTENT( OUT ) :: status

!  local variables

      LOGICAL :: there_are_params
      CHARACTER ( LEN = 80 ) :: line
      CHARACTER ( LEN = len_var ) :: var
      CHARACTER ( LEN = len_val ) :: val
      CHARACTER ( LEN = 2 ) :: typ
      CHARACTER, PARAMETER :: tab = CHAR(9)

!  run through the SIF file, looking for parameter lines

      WRITE( out, "( ' Parameter list:', / )" )
      there_are_params = .FALSE.
      DO
       line = REPEAT( ' ', 80 )
       READ( UNIT = in_sif, FMT = "( A80 )", END = 100, ERR = 100 ) line
       IF ( LEN( TRIM( line ) ) >= 50 ) THEN

!  this is a parameter line

         IF ( line( 40 : 50 ) == '$-PARAMETER' ) THEN
           var = ADJUSTL( line( 5 : 14 ) )
           val = ADJUSTL( line( 25 : 36 ) )
           typ = line( 2 : 3 )
           WRITE( out, "( A, '=', A, A, '(', A, ')', A )", advance="no" ) TRIM(var), TRIM(val), tab, typ, tab
           IF ( line( 1 : 1 )  == '*' ) THEN
             WRITE( out, "( A )", advance="no" ) tab
           ELSE
             WRITE( out, "( '-default value-' )", advance="no" )
           END IF
           IF ( LEN( TRIM( line( 51 : 80 ) ) ) > 0 ) THEN
             WRITE( out, "( A, 'comment: ', A )" ) tab, TRIM( line( 51:80 ) )
           ELSE
             WRITE( out, "( A, 'uncommented ' )" ) tab
           END IF
           there_are_params = .TRUE.
         END IF
       END IF
      END DO

  100 CONTINUE
      IF ( .NOT. there_are_params )                                            &
        WRITE( out, "( ' there are no parameters in the SIF file' )" )

      status = 0
      RETURN

!  end of subroutine SIFDECODER_list_parameters

    END SUBROUTINE SIFDECODER_list_parameters

!-*-*-*-  S I F D E C O D E R _ M O V E _ F I L E _ S U B R O U T I N E  -*-*-*-

    SUBROUTINE SIFDECODER_move_file( old, new, status )

!  preprocess a SIF file to give parameter values specified values

!  dummy arguments

      CHARACTER ( LEN = * ), INTENT( INOUT ) :: old, new
      INTEGER, INTENT( OUT ) :: status

!  local variables

      CHARACTER ( LEN = 72 ) :: line

!  open both old and new files for reading and writing

      OPEN( out_old, FILE = old, FORM = 'FORMATTED', STATUS = 'UNKNOWN' )
      REWIND out_old
      OPEN( out_new, FILE = new, FORM = 'FORMATTED', STATUS = 'UNKNOWN' )
      REWIND out_new

!  copy the data from the old to new file

      DO
       line = REPEAT( ' ', 72 )
       READ( UNIT = out_old, FMT = "( A72 )", END = 100, ERR = 100 ) line
       WRITE( out_new, "( A )" ) TRIM( line )
      END DO

!  delete the old file, and save the new one

  100 CONTINUE
      CLOSE( out_old, STATUS = 'DELETE' )
      CLOSE( out_new )

      status = 0
      RETURN

!  end of subroutine SIFDECODER_move_file

    END SUBROUTINE SIFDECODER_move_file

!  end of module SIFDECODER_standalone

  END MODULE SIFDECODER_standalone
