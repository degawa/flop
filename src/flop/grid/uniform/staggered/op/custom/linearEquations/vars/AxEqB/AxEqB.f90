!>連立方程式\(\boldsymbol{Ax}=\boldsymbol{b}\)
!>の取り扱いに関する派生型を提供する．
!>
!>派生型には，連立方程式の左辺\(\boldsymbol{Ax}\)と右辺\(\boldsymbol{b}\)
!>を取り扱う派生型が含まれる．
!>
!>@note
!>メインルーチンの中では明確に型宣言されない．
!>演算に使われる他の型の成分として宣言，演算子に渡す値として生成，
!>演算の結果として中間的に生成される．
!>@endnote
!>
module grid_uniform_staggered_op_custom_linEqs_vars_AxEqB
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_staggered_vars_scalar_2d
    use :: grid_uniform_staggered_op_custom_linEqs_vars_Ax_adt
    implicit none
    private

    !>連立方程式の左辺\(\boldsymbol{Ax}\)と
    !>右辺\(\boldsymbol{b}\)を表す派生型．
    type, public :: Ax_eq_b_type
        class(Ax_atype), allocatable :: Ax
            !! 連立方程式の左辺
        type(scalar_2d_type) :: b
            !! 連立方程式の右辺
    contains
        procedure, public, pass :: inverse
            !! 連立方程式を求解
        procedure, public, pass :: assign
            !! `Ax_eq_b_type`を代入
        generic :: assignment(=) => assign
    end type Ax_eq_b_type

contains
    !>左辺`Ax`と右辺`b`を用いて連立方程式を求解する．
    !>
    !>実際には，`.inverse.`演算子を利用して`.inverse.Ax_eq_b`と書き，
    !>演算子内からこの手続を呼ぶことを想定している．
    !>演算子を用いると被演算子を更新できないため，
    !>演算子は左辺`Ax`の成分`x`は，コピーするが直接更新はしない．
    subroutine inverse(this, unknown, rhs)
        implicit none
        !&<
        class(Ax_eq_b_type)     , intent(in)    :: this
            !! 当該実体仮引数
        class(scalar_2d_type)   , intent(inout) :: unknown
            !! 連立方程式の未知数
        class(scalar_2d_type)   , intent(in)    :: rhs
            !! 連立方程式の右辺
        !&>

        ! 実際の求解は，成分`Ax`の手続に委譲
        call this%Ax%solve(unknown, rhs)
    end subroutine inverse

    !>`Ax_eq_b_type`を代入する．
    !>
    !>単体で呼び出すことはなく，代入演算子をオーバーロードして利用する．
    subroutine assign(lhs, rhs)
        implicit none
        !&<
        class(Ax_eq_b_type), intent(out)    :: lhs
            !! 代入される`Ax_eq_b_type`<br>
            !! `=`演算子の左側に置かれる量<br>
            !! 当該実体仮引数
        class(Ax_eq_b_type), intent(in)     :: rhs
            !! 代入する`Ax_eq_b_type`<br>
            !! `=`演算子の右側に置かれる量
        !&>

        allocate (lhs%Ax, source=rhs%Ax)
        lhs%b = rhs%b
    end subroutine assign
end module grid_uniform_staggered_op_custom_linEqs_vars_AxEqB
