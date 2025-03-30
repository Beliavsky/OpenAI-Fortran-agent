module mixture_module
    implicit none
    integer, parameter :: dp = kind(1.0d0)
    integer, parameter :: max_iter = 1000
    real(kind=dp), parameter :: tol = 1.0e-6_dp

    type :: mixture_parameters
        real(kind=dp), allocatable :: weights(:)
        real(kind=dp), allocatable :: means(:)
        real(kind=dp), allocatable :: stddevs(:)
    end type mixture_parameters

contains

    pure function gaussian_pdf(x, mean, stddev) result(pdf)
        real(kind=dp), intent(in) :: x, mean, stddev
        real(kind=dp) :: pdf
        pdf = (1.0_dp / (sqrt(2.0_dp * 3.14159265358979323846_dp) * stddev)) * &
              exp(-0.5_dp * ((x - mean)/stddev)**2)
    end function gaussian_pdf

    subroutine display_parameters(params, title)
        implicit none
        type(mixture_parameters), intent(in) :: params
        character(*), intent(in) :: title
        integer :: i
        print *, title
        print '(A)', 'Comp | Weight    | Mean       | Std Dev    '
        do i = 1, size(params%weights)
            print '(I4, 2X, F10.6, 2X, F10.6, 2X, F10.6)', &
                  i, params%weights(i), params%means(i), params%stddevs(i)
        end do
    end subroutine display_parameters

    subroutine EM_fit(data, n, k, params)
        implicit none
        integer, intent(in) :: n, k
        real(kind=dp), intent(in) :: data(n)
        type(mixture_parameters), intent(out) :: params
        real(kind=dp), allocatable :: weights(:), means(:), stddevs(:)
        real(kind=dp), allocatable :: gamma_mat(:,:)
        integer :: iter, i, j
        real(kind=dp) :: log_likelihood, prev_log_likelihood, diff
        real(kind=dp) :: sum_gamma

        allocate(weights(k), means(k), stddevs(k), gamma_mat(n, k))
        weights = 1.0_dp / k
        means = data(1:k)
        stddevs = 1.0_dp
        iter = 0
        prev_log_likelihood = -1.0e20_dp

        do while (iter < max_iter)
            ! E-step
            do i = 1, n
                sum_gamma = 0.0_dp
                do j = 1, k
                    gamma_mat(i,j) = weights(j) * gaussian_pdf(data(i), means(j), stddevs(j))
                    sum_gamma = sum_gamma + gamma_mat(i,j)
                end do
                if (sum_gamma > 0.0_dp) then
                    gamma_mat(i, :) = gamma_mat(i, :) / sum_gamma
                else
                    gamma_mat(i, :) = 1.0_dp / k
                end if
            end do

            ! M-step
            do j = 1, k
                sum_gamma = sum(gamma_mat(:,j))
                weights(j) = sum_gamma / n
                means(j) = sum(gamma_mat(:,j) * data) / sum_gamma
                stddevs(j) = sqrt(sum(gamma_mat(:,j) * (data - means(j))**2) / sum_gamma)
            end do

            ! Compute log-likelihood
            log_likelihood = 0.0_dp
            do i = 1, n
                sum_gamma = 0.0_dp
                do j = 1, k
                    sum_gamma = sum_gamma + weights(j) * gaussian_pdf(data(i), means(j), stddevs(j))
                end do
                log_likelihood = log_likelihood + log(sum_gamma)
            end do

            diff = log_likelihood - prev_log_likelihood
            if (abs(diff) < tol) exit
            prev_log_likelihood = log_likelihood
            iter = iter + 1
        end do

        params%weights = weights
        params%means = means
        params%stddevs = stddevs
        deallocate(weights, means, stddevs, gamma_mat)
    end subroutine EM_fit

end module mixture_module

program test_mixture
    use mixture_module
    implicit none
    integer, parameter :: n = 1000
    integer, parameter :: k = 2
    integer :: i, component
    real(kind=dp), allocatable :: data(:)
    type(mixture_parameters) :: true_params, est_params
    real(kind=dp) :: u1, u2, z0
    integer :: seed_size
    integer, allocatable :: seed(:)

    ! Determine the required size for the seed array
    call random_seed(size=seed_size)
    allocate(seed(seed_size))

    ! Initialize the random seed
    call random_seed()

    ! Retrieve the current seed values
    call random_seed(get=seed)

    ! Define true parameters
    allocate(true_params%weights(k), true_params%means(k), true_params%stddevs(k))
    true_params%weights = (/ 0.3_dp, 0.7_dp /)
    true_params%means = (/ -2.0_dp, 3.0_dp /)
    true_params%stddevs = (/ 1.0_dp, 0.5_dp /)

    ! Simulate data using Box-Muller
    allocate(data(n))
    do i = 1, n
        call random_number(u1)
        if (u1 < true_params%weights(1)) then
            component = 1
        else
            component = 2
        end if
        call random_number(u1)
        call random_number(u2)
        z0 = sqrt(-2.0_dp * log(u1)) * cos(2.0_dp * 3.14159265358979323846_dp * u2)
        data(i) = true_params%means(component) + true_params%stddevs(component) * z0
    end do

    ! Fit EM
    call EM_fit(data, n, k, est_params)

    ! Display results
    call display_parameters(true_params, 'True Parameters:')
    call display_parameters(est_params, 'Estimated Parameters:')

    ! Deallocate allocated arrays
    deallocate(true_params%weights, true_params%means, true_params%stddevs, data, seed)
end program test_mixture