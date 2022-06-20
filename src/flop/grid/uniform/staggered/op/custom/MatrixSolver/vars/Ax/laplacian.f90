!>Laplace-Poisson方程式\(\nabla^2 x=b\)
!>の取り扱いに関する派生型および手続を提供する．
!>
!>派生型には，連立方程式の左辺\(\nabla^2 x\)を表す派生型が含まれる．
!>
!>手続には，Laplace方程式左辺のコンストラクタが含まれる．
!>
!>@note
!>メインルーチンの中では明確に型宣言されない．
!>演算に使われる他の型の成分として宣言，演算子に渡す値として生成，
!>演算の結果として中間的に生成される．
!>@endnote
!>
module grid_uniform_staggered_op_custom_solver_vars_Ax_laplacian
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
    contains
        procedure, public, pass :: eval
        !* \(\boldsymbol{Ax}\)を計算した結果を返却<br>
        ! 祖先型の手続をoverride

        procedure, public, pass :: assign
        !* `Ax_laplacian_type`を代入
        generic :: assignment(=) => assign

        procedure, public, pass :: construct_solver
        !* 連立方程式のソルバを設定．
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
        lhs%BC = rhs%BC
    end subroutine assign

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

    subroutine construct_solver(this, solver_spec)
        use :: grid_uniform_staggered_op_custom_solver_vars_solver_spec
        use :: grid_uniform_staggered_op_custom_solver_vars_solver
        implicit none
        class(Ax_laplacian_type), intent(inout) :: this
        class(solver_spec_atype), intent(in) :: solver_spec

        select type (solver_spec)
        !!-------------------------------------------------------------!
        type is (sor_spec_type) ! SOR
            allocate (laplacian_solver_sor_type :: this%solver)

            select type (solver => this%solver)
            type is (laplacian_solver_sor_type)
                solver%accel = solver_spec%get_acceleration_coefficient()
            end select
        !!-------------------------------------------------------------!
        type is (rbsor_spec_type) ! RBSOR
            allocate (laplacian_solver_rbsor_type :: this%solver)

            select type (solver => this%solver)
            type is (laplacian_solver_rbsor_type)
                solver%accel = solver_spec%get_acceleration_coefficient()
            end select
        end select
    end subroutine construct_solver
end module grid_uniform_staggered_op_custom_solver_vars_Ax_laplacian
