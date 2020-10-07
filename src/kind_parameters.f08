module kind_parameters ! Type kind parameter
    implicit none
    private ! by default every thing is private
    public :: rkind,ikind,ckind ! Make only selected public
    integer ,parameter :: digits=6  ! num. digits of kind
    integer ,parameter :: decades=9 ! num. representable decades
    integer ,parameter :: rkind = selected_real_kind(digits)
    integer ,parameter :: ikind = selected_int_kind(decades)
    integer ,parameter :: ckind = selected_char_kind('default')
end module kind_parameters