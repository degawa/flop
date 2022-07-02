!>ベクトル量に対するDirichlet境界条件に関係した型や手続を提供する．
!>
!>型には，ベクトル量の値を取り扱う型が含まれる．
!>
!>@note
!>メインルーチンの中では明確に型宣言されない．
!>演算に使われる他の型の成分として宣言，演算子に渡す値として生成，
!>演算の結果として中間的に生成される．
!>@endnote
!>
!>手続には，ベクトル量の値を取り扱う型を構築する
!>コンストラクタが含まれる．
!>
module grid_uniform_stg_op_custom_bc_vars_vector_value_Dirichlet
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: Dirichlet

    !>ベクトル量の値を取り扱う派生型．
    type, public :: Dirichlet_vector_value_type
        real(real64) :: value(2)
            !! 境界におけるベクトル量の値
            !! `=[x, y]`
    end type Dirichlet_vector_value_type

    !>ベクトル量を`Dirichlet([u_x, u_y])`
    !>で作成できるようにするためのインタフェース
    interface Dirichlet
        procedure :: Dirichlet_vector
    end interface

contains
    !>ベクトル量を扱う型を構築するためのコンストラクタ．
    function Dirichlet_vector(val) result(new_value)
        implicit none

        real(real64), intent(in) :: val(2)
            !! ベクトル量の値

        type(Dirichlet_vector_value_type) :: new_value
            !! 派生型にまとめられたベクトル量の値

        new_value%value = val
    end function Dirichlet_vector
end module grid_uniform_stg_op_custom_bc_vars_vector_value_Dirichlet
