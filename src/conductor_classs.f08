module conductor_class
    use kind_parameters ,only : ikind,rkind
    use differentiator_class ,only : differentiator
    implicit none
    private                 ! Hide everything by default
    public conductor        ! Expose type and type-bound procedures
    integer(ikind) ,parameter :: end_points=2

    type conductor
        private
        type(differentiator)                   :: diff
        real(rkind)                            :: diffusivity
        real(rkind) ,dimension(end_points)     :: boundary
        real(rkind) ,dimension(:) ,allocatable :: temp
                                                 !^ initial temperature
    contains
        procedure :: heat_for
        procedure :: temperature
        procedure :: time_derivative
    end type

    interface conductor
        procedure constructor
    end interface

contains
    type(conductor) function constructor(diff,prob,T_init)
        use conduction_module ,only : read_physics
        use problem_class ,only : problem
        type(differentiator) ,intent(in) :: diff
        type(problem) ,intent(in)        :: prob
        real(rkind), dimension(:) ,optional :: T_init
        
        constructor%diff = diff
        constructor%diffusivity = prob%diffusivity()
        constructor%boundary = prob%boundary_vals()

        if (present(T_init)) then
            if (size(T_init)/=prob%nodes()) stop 'In conductor: size mismatch.'
            constructor%temp = T_init
        else
            allocate(constructor%temp(prob%nodes()))
            ! if not input, set to one of the boudary value
            constructor%temp = constructor%boundary(1) 
        end if
    end function

    function temperature(this)
        class(conductor), intent(in) :: this
        real(rkind) ,dimension(:) ,allocatable :: temperature

        temperature = (/ this%boundary(1), this%temp, this%boundary(2) /)
    end function

    subroutine heat_for(this,dt) ! for inout of type
        class(conductor) ,intent(inout) :: this
        real(rkind) ,intent(in)         :: dt
        real(rkind) ,dimension(:), allocatable :: T_xx ! 2nd derivative

        allocate(T_xx(size(this%temp)))
        T_xx = this%diff%laplacian(this%temp,this%boundary)
        this%temp = this%temp + dt*this%diffusivity*T_xx
    end subroutine

    real(rkind) function time_derivative(this)
        class(conductor)        :: this
        real(rkind) ,dimension(:) ,allocatable :: T_xx_end
        real(rkind)             :: T_2

        if (size(this%temp)>1) then
            T_2 = this%temp(2)
        else
            T_2 = this%boundary(2)
        end if
        T_xx_end = this%diff%laplacian(this%temp, (/this%boundary(1),T_2/))
        time_derivative = this%diffusivity*T_xx_end(1)
    end function

end module