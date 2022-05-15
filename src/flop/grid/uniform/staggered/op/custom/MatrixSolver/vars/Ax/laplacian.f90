!>Laplace-Poisson方程式\(\nabla^2 x=b\)
!>の取り扱いに関する派生型を提供する．
!>
!>派生型には，連立方程式の左辺\(\nabla^2 x\)を表す派生型が含まれる．
!>
!>@note
!>メインルーチンの中では明確に型宣言されない．
!>演算に使われる他の型の成分として宣言，演算子に渡す値として生成，
!>演算の結果として中間的に生成される．
!>@endnote
!>
module grid_uniform_staggered_op_custom_solver_Ax_vars_laplacian
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_staggered_2d
    use :: grid_uniform_staggered_vars_scalar_2d
    use :: grid_uniform_staggered_op_custom_solver_vars_Ax_adt
    use :: grid_uniform_staggered_op_custom_bc_impose
    implicit none
    private
    public :: laplacian

    !>Laplace-Poisson方程式の左辺\(\nabla^2 x\)を
    !>取り扱う派生型．
    type, public, extends(Ax_atype) :: Ax_laplacian_type
        real(real64) :: accel = 1.925d0
            !! 解放にSOR法を利用する場合の加速係数
    contains
        procedure, public, pass :: solve
            !! 連立方程式を解いて未知数`x`を更新<br>
            !! 祖先型の手続をoverride
        procedure, public, pass :: eval
            !! \(\boldsymbol{Ax}\)を計算した結果を返却<br>
            !! 祖先型の手続をoverride

        procedure, public, pass :: assign
            !! `Ax_laplacian_type`を代入
        generic :: assignment(=) => assign
    end type Ax_laplacian_type

    !>`Ax_laplacian_type`のコンストラクタを
    !>`laplacian(p)`と呼ぶためのインタフェース．
    interface laplacian
        procedure :: construct_Ax_laplacian
    end interface

contains
    !>`Ax_laplacian_type`のコンストラクタ．
    function construct_Ax_laplacian(x) result(new_Ax_lap)
        implicit none

        class(scalar_2d_type), intent(in) :: x
            !! 連立方程式の未知数に相当するスカラ量

        type(Ax_laplacian_type) :: new_Ax_lap
            !! Laplace-Poisson方程式の左辺

        ! 未知数`x`のみを更新
        call new_Ax_lap%x%construct(x%get_base_grid())
        new_Ax_lap%x = x

        ! 境界条件および許容誤差は，実体を構築した後，
        ! 別途演算子を用いて更新
    end function construct_Ax_laplacian

    !>`Ax_laplacian_type`を代入する．
    !>
    !>単体で呼び出すことはなく，代入演算子をオーバーロードして利用する．
    subroutine assign(lhs, rhs)
        implicit none
        !&<
        class(Ax_laplacian_type), intent(out)   :: lhs
            !! 代入される`Ax_laplacian_type`<br>
            !! `=`演算子の左側に置かれる量<br>
            !! 当該実体仮引数
        class(Ax_laplacian_type), intent(in)    :: rhs
            !! 代入する`Ax_laplacian_type`<br>
            !! `=`演算子の右側に置かれる量
        !&>

        lhs%x = rhs%x
        lhs%err_tol = rhs%err_tol
        lhs%BC = rhs%BC
        lhs%accel = rhs%accel
    end subroutine assign

    !>連立方程式を解いて未知数`x`を更新する．
    !>
    !>求解アルゴリズムにはSOR法を用いる．
    subroutine solve(this, x, b)
        implicit none
        !&<
        class(Ax_laplacian_type), intent(in)    :: this
            !! 連立方程式の左辺\(\boldsymbol{Ax}\)の情報<br>
            !! 当該実体仮引数
        class(scalar_2d_type)   , intent(inout) :: x
            !! 未知数
        class(scalar_2d_type)   , intent(in)    :: b
            !! 右辺
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
        do while (err_r > this%err_tol)
            ite_SOR = ite_SOR + 1

            err_r = 0d0
            err_n = 0d0
            err_d = 0d0

            !&<
            do jc = 1, Ncy
            do ic = 1, Ncx
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
            call impose(x, this%BC)

            if (err_d <= epsilon(err_d)) err_d = 1d0
            err_r = sqrt(err_n/err_d)
        end do
    end subroutine solve

    !>\(\nabla^2 x\)を計算した結果を返す．
    function eval(this) result(new_b)
        use :: grid_uniform_staggered_op_unary_laplacian_acc2
        implicit none

        class(Ax_laplacian_type), intent(in) :: this
            !! 連立方程式の左辺<br>
            !! 当該実体仮引数

        type(scalar_2d_type) :: new_b
            !! 計算された右辺

        call new_b%construct(this%x%get_base_grid()) !返値を構築

        new_b = .laplacian.this%x ! \(\nabla^2 x\)なのでLaplace演算子を流用
    end function eval
end module grid_uniform_staggered_op_custom_solver_Ax_vars_laplacian
