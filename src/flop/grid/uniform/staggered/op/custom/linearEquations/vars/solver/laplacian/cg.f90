!>連立方程式\(\boldsymbol{Ax}=\boldsymbol{b}\)
!>の取り扱いに関する派生型を提供する．
!>
!>派生型には，Laplace-Poisson方程式から作られる連立方程式を
!>共役勾配法（CG法）で求解するソルバを表す派生型が含まれる．
!>
module grid_uniform_stg_op_cust_linEqs_vars_solver_lap_CG
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_stg_vars_scalar_2d
    use :: grid_uniform_stg_vars_scalar_2d_bc
    use :: grid_uniform_stg_op_cust_bc_impose
    use :: grid_uniform_stg_op_cust_linEqs_vars_solver_adt
    implicit none
    private

    !>Laplace-Poisson方程式をCG法で解くソルバを表す派生型．
    type, public, extends(solver_atype) :: laplacian_solver_cg_type
        ! 補助ベクトルをここで宣言する予定であったが，
        ! 当該実体仮引数を`intent(in)`で宣言しているため
        ! 成分を変更できない．
    contains
        procedure, public, pass :: solve_using_iterative_method
        !* Poisson方程式を解いて未知数`x`を更新
    end type laplacian_solver_cg_type

contains
    !>CG法を用いてPoisson方程式を解いて未知数`x`を更新する．
    subroutine solve_using_iterative_method(this, x, b, BC, err_tol)
        use :: grid_uniform_stg_2d
        use :: grid_uniform_stg_op_unary_laplacian_acc2
        use :: grid_uniform_stg_op_binary
        use :: grid_uniform_stg_op_unary
        implicit none
        !&<
        class(laplacian_solver_cg_type)         , intent(in)    :: this
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
        type(scalar_2d_type) :: p, r, Ap

        integer(int32) :: ite_CG
        real(real64) :: rr, bb, err, scale_x, scale_r, p_dot_Ap

        ! 補助ベクトルは手続呼出し毎に組み立てる
        grid => x%get_base_grid()
        call p%construct(grid)
        call r%construct(grid)
        call Ap%construct(grid)

        r = b - .laplacian.x

        bb = norm2(b); if (bb <= epsilon(bb)) bb = 1d0
        rr = norm2(r)
        err = rr/bb
        if (err < err_tol) return

        scale_r = 0d0

        ite_CG = 0
        do while (err > err_tol)
            ite_CG = ite_CG + 1

            p = r + scale_r*p
            call impose(p, BC)

            Ap = .laplacian.p

            p_dot_Ap = p.dot.Ap
            scale_x = (r.dot.r)/p_dot_Ap

            x = x + scale_x*p
            r = r - scale_x*Ap
            call impose(x, BC)

            rr = r.dot.r
            err = rr/bb

            scale_r = rr/(scale_x*p_dot_Ap)
        end do
    end subroutine solve_using_iterative_method
end module grid_uniform_stg_op_cust_linEqs_vars_solver_lap_CG
