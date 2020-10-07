module differentiator_class

    use kind_parameters ,only : ikind,rkind
    implicit none
    private                  ! Hide everything by default
    public :: differentiator ! Expose type/constructor/type-bound procs.

    type differentiator
        private
        real(rkind),dimension(:,:),allocatable::diff_matrix
    contains
        procedure :: laplacian ! return Laplacian
        procedure :: lap_matrix ! return Laplacian matrix operator
    end type

    interface differentiator
        procedure constructor
    end interface
contains
    type(differentiator) function constructor(spec)
        use problem_class ,only : problem
        use conduction_module ,only : differencing_stencil
        type(problem) ,intent(in) :: spec
        integer(ikind) ,parameter :: diagonals=3

        allocate(constructor%diff_matrix(spec%nodes(),diagonals))

        constructor%diff_matrix = &
            differencing_stencil(spec%spacing(),spec%nodes())
    end function

    pure function laplacian(this,T,Tboundaries)
        use conduction_module ,only : differentiate
        class(differentiator) ,intent(in)      :: this ! use the same class
        real(rkind) ,dimension(:) ,intent(in)  :: T
        real(rkind) ,dimension(:) ,allocatable :: laplacian
        integer(ikind) ,parameter              :: end_points=2
        real(rkind) ,dimension(end_points) ,intent(in) :: Tboundaries

        allocate(laplacian(size(T)))
        laplacian = differentiate( &
            this%diff_matrix,T,Tboundaries(1),Tboundaries(2))
        
    end function

    pure function lap_matrix(this)
        class(differentiator) ,intent(in)     :: this ! use the same class
        real(rkind) ,dimension(:,:) ,allocatable :: lap_matrix

        lap_matrix = this%diff_matrix
        
    end function

end module