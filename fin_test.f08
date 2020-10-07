program fin_test

    use iso_fortran_env ,only : input_unit
    use kind_parameters ,only : rkind
    use problem_class   ,only : problem
    use differentiator_class ,only : differentiator
    use conductor_class ,only : conductor
    implicit none

    real(rkind) ,parameter :: toterance=1.0E-06
    type(problem)          :: specification
    type(conductor)        :: fin
    type(differentiator)   :: finite_difference

    specification          = problem(input_unit)
    finite_difference      = differentiator(specification)
    fin                    = conductor(finite_difference,specification)
    print '(a,5g9.3)','initial temperature = ',fin%temperature()
    call fin%heat_for(specification%time_step())
    print '(a,5g9.3)','final temperature = ',fin%temperature()

    if (abs(fin%time_derivative())<toterance) then
        print '(2(a,es9.3))','|dT/dt|=',fin%time_derivative(),'<',toterance
        print *, 'In main: test passed. :)'
    else
        print '(2(a,es9.3))','|dT/dt|=',fin%time_derivative(),'>',toterance
        print *, 'In main: test failed. :('    
    end if
    
end program fin_test