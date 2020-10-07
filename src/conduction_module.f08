module conduction_module
    use kind_parameters ,only : rkind,ikind
    implicit none                ! tri-diagonal array positions: 
    integer(ikind) ,parameter :: diagonals=3,low_diag=1,diag=2,up_diag=3
contains
    ! pure forces the check of intent
    pure logical function found(namelist_io)
        integer(ikind) ,intent(in) :: namelist_io
        if (namelist_io == 0) then
            found = .true.
        else
            found = .false.
        end if
    end function

    logical function read_physics(unit,alpha,L_fin,T_chip,T_air)
        real(rkind)    ,intent(out) :: alpha,L_fin,T_chip,T_air
        integer(ikind) ,intent(in)  :: unit
        integer(ikind)              :: physics_io     
        namelist /physics/ alpha,L_fin,T_chip,T_air ! namelist reades in order from e.g file 
        logical                     :: file_exists

        !print *, 'In conduction_module reading physics file'
        !open(unit,file='input.dat')
        inquire(unit,exist=file_exists)
        if (file_exists) then
            open(unit,file='input.dat')
            read(unit,nml=physics,iostat=physics_io)
            close(unit)
        end if
        !read(unit,nml=physics,iostat=physics_io)
        read_physics = found(physics_io)
    end function

    logical function read_numerics(unit,dt,nodes,dx,L_fin)
        real(rkind)    ,intent(out) :: dt,dx
        integer(ikind) ,intent(out) :: nodes
        real(rkind)    ,intent(in)  :: L_fin
        integer(ikind) ,intent(in)  :: unit
        integer(ikind)              :: numerics_io,elements
        namelist /numerics/ dt,nodes

        !print *, 'In conduction_module reading numerics input from file'
        open(unit,file='input.dat') ! already open
        read(unit,nml=numerics,iostat=numerics_io)
        !print *, 'iostat n', numerics_io
        read_numerics = found(numerics_io)
        elements = nodes + 1
        dx = L_fin/elements
        !rewind(unit)
        close(unit)
    end function

    pure real(rkind) function stable_dt(dx,alpha)
        real(rkind)   ,intent(in) :: dx,alpha
        real(rkind)   ,parameter  :: safety_factor=0.9
        stable_dt = safety_factor*(dx**2/alpha)
    end function

    pure function differencing_stencil(dx,nodes) result(central_diff)
        real(rkind)     ,intent(in) :: dx
        integer(ikind) ,intent(in) :: nodes
        real(rkind),dimension(:,:),allocatable :: central_diff
        
        allocate(central_diff(nodes,diagonals))
        central_diff(:,low_diag) = 1.0/dx**2
        central_diff(:,    diag) = -2.0/dx**2
        central_diff(:, up_diag) = 1.0/dx**2
    end function

    pure function differentiate(finite_diff,T,T1st,Tlast) result(T_xx)
        real(rkind),dimension(:) ,intent(in)  :: T
        real(rkind),dimension(:,:) ,intent(in)  :: finite_diff ! differentiation op
        real(rkind)              ,intent(in)  :: T1st,Tlast
        real(rkind),dimension(:) ,allocatable :: T_xx
        integer(ikind)                        :: nodes, i

        allocate(T_xx(nodes))
        T_xx(1) =  finite_diff(1,low_diag)*T1st &
                  +finite_diff(1,    diag)*T(1) &
                  +finite_diff(1, up_diag)*T(2)
        forall(i=2:nodes-1)
            T_xx(i) =  finite_diff(i,low_diag)*T(i-1) &
                      +finite_diff(i,    diag)*T(i) &
                      +finite_diff(1, up_diag)*T(i+1)
        end forall 
        T_xx(nodes) =  finite_diff(nodes,low_diag)*T(nodes-1) &
                      +finite_diff(nodes,    diag)*T(nodes) &
                      +finite_diff(nodes, up_diag)*Tlast
    end function

end module conduction_module