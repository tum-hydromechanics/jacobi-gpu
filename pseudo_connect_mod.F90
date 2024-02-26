
module pseudo_connect_mod

    use mpi_f08
    use ISO_FORTRAN_ENV

#if _OPENACC
    use openacc
    ! use nvtx, only: nvtxrangepushaargb, nvtxrangepop
#endif

    implicit none

    PRIVATE

    ! Lists that hold the send and receive request arrays
    integer, parameter :: nPacks = 1
    integer, parameter :: nSend = 2 * nPacks
    integer, parameter :: nRecv = 2 * nPacks

    type(MPI_Request) :: sendReqs(nSend), recvReqs(nRecv)
    type(MPI_Status) :: sendStatusList(nSend), recvStatusList(nRecv)
    type(MPI_Datatype) :: testType
    integer :: nend, nstart, myid, left, right, mbuf, length, pStart

    ! Pointer
    real, pointer :: pA(:,:)

    ! Arrays
    real, allocatable :: sendbuf(:,:), recvbuf(:,:)

    public :: connect, connect_init, connect_exit

CONTAINS

    subroutine connect_init( m, n )

        integer, intent(in) :: m, n

        integer :: ierr, numprocs, nchunk
        integer(kind=MPI_Address_kind) :: realBytes

        call MPI_Comm_rank( MPI_COMM_WORLD, myid, ierr )
        call MPI_Comm_size( MPI_COMM_WORLD, numprocs, ierr )

        ! setting periodic boundaries
        left = myid - 1
        if( myid == 0 ) left = numprocs - 1
        right = myid + 1
        if( myid == numprocs - 1 ) right = 0

        ! testType = very simple type
        CALL MPI_Type_contiguous( M / nPacks, MPI_REAL, testType, ierr )
        CALL MPI_Type_commit( testType, ierr )

        ! buffers are allocated
        mbuf = m
        ALLOCATE( sendbuf(mbuf,2), recvbuf(mbuf,2) )

        ! start into the unstructured data region

        !$omp target enter data map(alloc:sendbuf,recvbuf)

    end subroutine connect_init



    subroutine connect_exit

        ! exiting the unstructured data region

        !$omp target exit data map(delete:sendbuf,recvbuf)

        DEALLOCATE( sendbuf, recvbuf )

    end subroutine connect_exit



    subroutine connect( A, m, nst, nen )

        integer, intent(in) :: m, nst, nen
        real, intent(inout), target :: A(m,(nst-1):(nen+1))

        integer :: ierr, numprocs, nchunk

        integer, allocatable :: writeList(:)
        integer, allocatable :: readList(:)

        integer :: i, section, idx
        type(MPI_Status) :: stat

        ! storing call parameter
        nstart = nst
        nend = nen

        ! allocating the lists
        ALLOCATE( writeList(nPacks), readList(nPacks) )


        ! filling the lists with random stuff (not contiguous)
        IF ( nPacks == 1 ) THEN
            section = mbuf
            writeList(1) = 1
            readList(1) = 1
        ELSE
            section = mbuf / nPacks
            DO i = 1, nPacks / 2
                writeList( 2*i-1 ) = (2*i-1) * section + 1
                writeList( 2*i-0 ) = (2*i-2) * section + 1
                readList( 2*i-1 ) = (2*i-1) * section + 1
                readList( 2*i-0 ) = (2*i-2) * section + 1
            END DO
        END IF

        ! setting the pointer
        pA => A

        DO i = 1, nPacks
            pStart = readList(i)
            CALL recv( i, pStart )
        END DO

        DO i = 1, nPacks
            ! setting the pointers
            pStart = writeList(i)
            length = section
            CALL write_buffer( pstart, length )
        END DO

        ! performing the communication with the type of choice
        DO i = 1, nPacks
            pStart = writeList(i)
            CALL send( i, pStart )
        END DO

        ! waiting for communication to finish (recv)
        CALL MPI_Waitall( nRecv, recvReqs, recvStatusList, ierr )

        DO i = 1, nPacks
            ! setting the pointers
            pStart = readList(i)
            length = section
            CALL read_buffer( pstart, length )
        END DO

        ! waiting for communication to finish (send)
        CALL MPI_Waitall( nSend, sendReqs, sendStatusList, ierr )

        ! releasing the pointer
        NULLIFY( pA )

        ! deallocating the lists
        DEALLOCATE( writeList, readList )

    end subroutine connect



    subroutine write_buffer( pstart, length )

        integer, intent(IN) :: pstart, length
        integer :: i

        ! call nvtxrangepushaargb("BufWrite"//char(0),int(z'F781F3',4)) ! pink

        ! pStart is set from outside the function
        ! length is set from outside the function

        !$omp target parallel do
        DO i = 0, length-1
            sendbuf( pStart+i, 1 ) = pA( pStart+i,nstart )
            sendbuf( pStart+i, 2 ) = pA( pStart+i,nend )
        END DO
        !$omp end target parallel do

        ! call nvtxrangepop

    end subroutine write_buffer



    subroutine read_buffer( pstart, length )

        integer, intent(IN) :: pstart, length
        integer :: i

        ! pStart is set from outside the function
        ! length is set from outside the function

        !$omp target parallel do
        DO i = 0, length-1
            pA( pStart+i, nend+1 ) = recvbuf( pStart+i, 1 )
            pA( pStart+i, nstart-1 ) = recvbuf( pStart+i, 2 )
        END DO
        !$omp end target parallel do

    end subroutine read_buffer




    subroutine send( i, pstart )

        integer :: ierr
        integer, intent(in) :: pstart, i

        ! sendbuf(1,1) = start of pA(:,nstart)
        ! sendbuf(1,2) = start of pA(:,nend)

        !$omp target data use_device_addr(sendbuf)
        CALL MPI_Isend( sendbuf(pstart,1), 1, testType, left,  1, MPI_COMM_WORLD, sendReqs(0*nPacks+i), ierr )
        CALL MPI_Isend( sendbuf(pstart,2), 1, testType, right, 2, MPI_COMM_WORLD, sendReqs(1*nPacks+i), ierr )
        !$omp end target data

    end subroutine send



    subroutine recv( i, pstart )

        integer :: ierr
        integer, intent(in) :: pstart, i

        ! recvbuf(1,1) = start of pA(:,nend+1)
        ! recvbuf(1,2) = start of pA(:,nstart-1)

        !$omp target data use_device_addr(recvbuf)
        CALL MPI_Irecv( recvbuf(pstart,1), 1, testType, right, 1, MPI_COMM_WORLD, recvReqs(0*nPacks+i), ierr )
        CALL MPI_Irecv( recvbuf(pstart,2), 1, testType, left,  2, MPI_COMM_WORLD, recvReqs(1*nPacks+i), ierr )
        !$omp end target data

    end subroutine recv


end module pseudo_connect_mod

! Code was translated using: /home/ga46fih2/intel-application-migration-tool-for-openacc-to-openmp/src/intel-application-migration-tool-for-openacc-to-openmp -overwrite-input pseudo_connect_mod.F90
