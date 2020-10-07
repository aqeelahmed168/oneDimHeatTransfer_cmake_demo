module problem_class
    use kind_parameters ,only: ikind,rkind
    implicit none
    private
    public :: problem

    type problem
        private
        integer(ikind) :: num_nodes=4
        real(rkind)    :: air_temp=1.0,chip_temp=1.0 ! boundary temperatures
        real(rkind)    :: alpha=1.0,length=1.0       ! physical parameters
        real(rkind)    :: dt=0.1,dx                  ! numerical parameters
                                !^ constructor computes default
    contains
        procedure :: boundary_vals
        procedure :: diffusivity
        procedure :: nodes
        procedure :: time_step
        procedure :: spacing
    end type

    interface problem
        ! constructor spec maping to problem
        procedure spec
    end interface

contains

    type(problem) function spec(file)

        use conduction_module ,only : read_physics,read_numerics,stable_dt
        integer(ikind) :: file
        integer(ikind) :: elements_default

        elements_default = spec%num_nodes+1
        spec%dx = spec%length/elements_default ! default element size

        if (.not. read_physics(file &
              ,spec%alpha,spec%length,spec%chip_temp,spec%air_temp)) &
              print *, 'In problem constructor: Using default physics specification.'
 
        if (.not. read_numerics(file &
              ,spec%dt,spec%num_nodes,spec%dx,spec%length)) &
            print *, 'In problem constructor: Using default numerical specifications.'

        spec%dt = min(spec%dt,stable_dt(spec%dx,spec%alpha))

    end function

    pure function boundary_vals(this)
        class(problem) ,intent(in) :: this ! way to call the type in procedure
        integer(ikind) ,parameter :: end_points=2
        real(rkind) ,dimension(end_points) :: boundary_vals
        boundary_vals = (/this%chip_temp,this%air_temp/) ! array of 2 elems
    end function

    pure real(rkind) function diffusivity(this)
        class(problem) ,intent(in) :: this
        diffusivity = this%alpha
    end function

    pure integer(ikind) function nodes(this)
        class(problem) ,intent(in) :: this
        nodes = this%num_nodes
    end function

    pure real(rkind) function time_step(this)
        class(problem) ,intent(in) :: this
        time_step = this%dt
    end function

    pure real(rkind) function spacing(this)
        class(problem) ,intent(in) :: this
        spacing = this%dx
    end function

end module problem_class