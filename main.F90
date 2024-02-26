
program main

    use mpi_f08
    use pseudo_connect_mod, only: connect, connect_init, connect_exit

#if _OPENMP
    use omp_lib
    ! OMP runtime lib e.g get_num_device
    ! use nvtx, only: nvtxrangepushaargb, nvtxrangepop
#endif

    implicit none

    ! parameter
    integer :: N = 1024 ! > influences domain size
    integer :: M = 1024 ! > influences MPI message size

    integer :: iter_max = 500
    real, parameter :: tol = 0.00001

    character(len=32) :: num1char, num2char, num3char

    ! arrays
    real, allocatable, target :: A(:,:)
    real, allocatable, target :: Anew(:,:)

    ! pointers
    real, pointer :: pA(:,:)
    real, pointer :: pAnew(:,:)

    ! scalars
    integer :: ierr, myid, numprocs, ndevice, idevice, nchunk
    integer :: nstart, nend, iter
    double precision :: wtime, stime, etime, mpi_starttime, reftime, partime
    real :: error, glob_error

    !!! --- program body --- !!!

    ! MPI start
    call MPI_Init( ierr )
    mpi_starttime = MPI_Wtime()

    call MPI_Comm_rank( MPI_COMM_WORLD, myid, ierr )
    call MPI_Comm_size( MPI_COMM_WORLD, numprocs, ierr )
    if ( myid == 0 ) write(*,*) " - successful MPI initialization"

    ndevice = 0

#if _OPENMP
    ndevice = omp_get_num_devices()
    ! WRITE(*,*) "ndevice = ", ndevice
    ! consider moving to block allocation
    idevice = mod( myid, ndevice )
    call omp_set_default_device( idevice )
    write(*,*) "I am rank", myid, " and my device is ", idevice

    ! empty target region causes initialization
    !$omp target
    !$omp end target
    ! afterwards lazy init has been done for sure
#endif

    if ( myid == 0 ) write(*,*) " - detected ", ndevice, " devices"

    ! accepting problem parameters
    IF( COMMAND_ARGUMENT_COUNT() /= 3 )THEN
        ! output for correct usage
        WRITE(*,*) "Please, submit 3 arguments: dimN, dimM, iterations"
        WRITE(*,*) "Continuing with default values."
    ELSE
        ! reading the arguments as integers
        CALL GET_COMMAND_ARGUMENT( 1, num1char )
        READ( num1char,'(i9)' ) M
        CALL GET_COMMAND_ARGUMENT( 2, num2char )
        READ( num2char,'(i9)' ) N
        CALL GET_COMMAND_ARGUMENT( 3, num3char )
        READ( num3char,'(i9)' ) iter_max
    END IF

    if ( mod( N, numprocs ) /= 0 ) then
        write(*,*) "invalid number of procs"
        call exit( 0 )
    end if

    ! distribution of chunks
    nchunk = N / numprocs
    nstart = max( myid * nchunk + 1, 2 )
    nend = min( (myid + 1) * nchunk, N-1 )

    if ( myid == 0 ) write(*,*) "domain size of ", M, " x ", N

    ! --- MPI-parallel solution

    ! only allocate the section with global index range
    allocate( A(m,(nstart-1):(nend+1)) )
    allocate( Anew(m,(nstart-1):(nend+1)) )

    ! setting initial and boundary conditions (resetting)
    A = 0.0; Anew = 0.0
    nullify( pA )
    nullify( pAnew )

    call set_boundary_conditions( M, nstart, nend, N, A )
    call set_boundary_conditions( M, nstart, nend, N, Anew )

    ! preparing the parallel run
    call connect_init( M, N )

    call MPI_Barrier( MPI_COMM_WORLD, ierr )
    if ( myid == 0 ) write(*,*) " - computing parallel solution"
    if ( myid == 0 ) write(*,*) " - with NPROCS =", numprocs
    if ( myid == 0 ) write(*,*) " - with NDEVICE =", ndevice
    call MPI_Barrier( MPI_COMM_WORLD, ierr )

    stime = MPI_Wtime()
    iter = 0; error = 999.9

    !$omp target data map(tofrom:A,Anew,pA,pAnew) map(to:myid,numprocs)

    do while ( error > tol .and. iter < iter_max )

        ! swapping the pointers
        if ( modulo( iter, 2 ) == 0 ) then
            pA => A
            pAnew => Anew
        else
            pA => Anew
            pAnew => A
        end if

        ! executing the stencil operation 
        call stencil( pA, pAnew, m, nstart, nend, error )
        call MPI_Allreduce( MPI_IN_PLACE, error, 1, MPI_REAL, MPI_MAX, MPI_COMM_WORLD, ierr )

        ! implementation of hand-made buffered send
        call connect( pAnew, M, nstart, nend )

        if( myid == 0 .and. mod( iter, 100 ) == 0 ) then
            write(*,*) iter, error
        end if
        iter = iter + 1

    end do

    !$omp end target data

    call MPI_Barrier( MPI_COMM_WORLD, ierr )
    etime = MPI_Wtime(); partime = (etime - stime)
    if ( myid == 0 ) write(*,*) " - done: Time = ", partime

    WRITE(*,*) maxval( A ), minval( A )

    nullify( pA, pAnew )
    deallocate( A, Anew )
    call connect_exit()

    call MPI_Finalize( ierr )

end program




!!! --- functions --- !!!

subroutine stencil( A, Anew, m, nstart, nend, error )

    implicit none

    integer, intent(in) :: m, nstart, nend
    real, intent(in) :: A(m,(nstart-1):(nend+1))

    real, intent(inout) :: Anew(m,(nstart-1):(nend+1))
    real, intent(inout) :: error

    integer :: i, j

    error = 0.0

    !$omp target teams distribute parallel do collapse(2) reduction(max:error) map(tofrom:error)
    do j = nstart, nend
         do i = 2, m-1
             ! executing the stencil operation
             Anew(i,j) = 0.25 * ( A(i+1,j) + A(i-1,j) + A(i,j+1) + A(i,j-1) )
             error = max( error, abs( Anew(i,j)-A(i,j) ))
        end do
    end do
    !$omp end target teams distribute parallel do

end subroutine stencil



subroutine set_boundary_conditions( m, nstart, nend, ntot, array )

    implicit none

    integer, intent(in) :: m, nstart, nend, ntot
    real, intent(inout) :: array(m,(nstart-1):(nend+1))

    integer :: iter
    real, parameter :: pi = 4.0 * atan( 1.0 )
    real :: dx, xval

    dx = 1.0 / real( ntot )

    do iter = (nstart-1), (nend+1)
        xval = ( 0.5 + 1.0*(iter-1) ) * dx
        array(1,iter) = SIN( 2.0 * pi * xval )
        array(m,iter) = SIN( 2.0 * pi * xval )
    end do

end subroutine set_boundary_conditions

! Code was translated using: /home/ga46fih2/intel-application-migration-tool-for-openacc-to-openmp/src/intel-application-migration-tool-for-openacc-to-openmp -overwrite-input -force-backup main.F90
