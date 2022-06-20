!>連立方程式\(\boldsymbol{Ax}=\boldsymbol{b}\)
!>の取り扱いに関する派生型を提供する．
!>
!>派生型には，Lapace-Poisson方程式から作られる連立方程式を
!>RBSOR法で求解するソルバを表す派生型が含まれる．
!>
module grid_uniform_staggered_op_custom_solver_vars_solver_lap_RBSOR
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_staggered_vars_scalar_2d
    use :: grid_uniform_staggered_vars_scalar_2d_bc
    use :: grid_uniform_staggered_op_custom_bc_impose
    use :: grid_uniform_staggered_op_custom_solver_vars_solver_adt
    implicit none
    private

    !>Laplace-Poisson方程式をRed-Black SOR法で解くソルバを表す派生型．
    type, public, extends(solver_atype) :: laplacian_solver_rbsor_type
        real(real64) :: accel = 1d0
            !! SOR法の加速係数
    contains
        procedure, public, pass :: solve_using_iterative_method
        !* Poisson方程式を解いて未知数`x`を更新
    end type laplacian_solver_rbsor_type

contains
    !>Red-Black SOR法を用いてPoisson方程式を解いて未知数`x`を更新する．
    subroutine solve_using_iterative_method(this, x, b, BC, err_tol)
        use :: grid_uniform_staggered_2d
        implicit none
        !&<
        class(laplacian_solver_rbsor_type)      , intent(in)    :: this
            !! 当該実体仮引数
        class(scalar_2d_type)                   , intent(inout) :: x
            !! 未知数
        class(scalar_2d_type)                   , intent(in)    :: b
            !! 右辺
        class(scalar_boundary_condition_type)   , intent(in)    :: BC
            !! 境界条件
        real(real64)                            , intent(in)    :: err_tol
            !! 許容誤差
        !&>

        type(staggered_uniform_grid_2d_type), pointer :: grid

        integer(int32) :: ic, jc, Ncx, Ncy
        real(real64) :: dx, dy, dxdx, dydy, dxdxdydy, dxdy2
        integer(int32) :: ite_SOR
        real(real64) :: err_n, err_d, err_r, d_f

        ! 格子の情報の取得
        grid => x%get_base_grid()
        call grid%get_number_of_grid_center_to(Ncx, Ncy)
        call grid%get_interval_to(dx, dy)

        ! 計算用パラメータの設定
        dxdx = dx*dx
        dydy = dy*dy
        dxdxdydy = dxdx*dydy
        dxdy2 = (dxdx + dydy)*2d0

        ite_SOR = 0
        err_r = huge(err_r)
        do while (err_r > err_tol)
            ite_SOR = ite_SOR + 1

            err_r = 0d0
            err_n = 0d0
            err_d = 0d0

            !&<
            do jc = 1, Ncy
            do ic = 1+mod(jc,2), Ncx, 2
                d_f = (  dydy*(x%val(ic-1, jc  ) + x%val(ic+1, jc  )) &
                       + dxdx*(x%val(ic  , jc-1) + x%val(ic  , jc+1)) &
                       - (dxdxdydy*b%val(ic, jc)) &
                      )/(dxdy2) - x%val(ic, jc)
                x%val(ic, jc) = x%val(ic, jc) + this%accel*d_f
                err_n = err_n + d_f**2
                err_d = err_d + x%val(ic, jc)**2
            end do
            end do
            do jc = 1, Ncy
            do ic = 2-mod(jc,2), Ncx, 2
                d_f = (  dydy*(x%val(ic-1, jc  ) + x%val(ic+1, jc  )) &
                       + dxdx*(x%val(ic  , jc-1) + x%val(ic  , jc+1)) &
                       - (dxdxdydy*b%val(ic, jc)) &
                      )/(dxdy2) - x%val(ic, jc)
                x%val(ic, jc) = x%val(ic, jc) + this%accel*d_f
                err_n = err_n + d_f**2
                err_d = err_d + x%val(ic, jc)**2
            end do
            end do
            !&>

            !ノイマン境界条件を適用
            call impose(x, BC)

            if (err_d <= epsilon(err_d)) err_d = 1d0
            err_r = sqrt(err_n/err_d)
        end do
    end subroutine solve_using_iterative_method
end module grid_uniform_staggered_op_custom_solver_vars_solver_lap_RBSOR
