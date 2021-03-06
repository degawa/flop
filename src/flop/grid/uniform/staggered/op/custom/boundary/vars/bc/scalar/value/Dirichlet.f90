!>スカラ量に対するDirichlet境界条件に関係した型や手続を提供する．
!>
!>型には，スカラ量の値を取り扱う型が含まれる．
!>
!>@note
!>メインルーチンの中では明確に型宣言されない．
!>演算に使われる他の型の成分として宣言，演算子に渡す値として生成，
!>演算の結果として中間的に生成される．
!>@endnote
!>
!>手続には，スカラ量の値を取り扱う型を構築する
!>コンストラクタが含まれる．
!>
module grid_uniform_stg_op_cust_bc_vars_scalar_value_Dirichlet
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: Dirichlet

    !>スカラ量の値を取り扱う派生型．
    type, public :: Dirichlet_scalar_value_type
        real(real64) :: value
            !! 境界におけるスカラ量の値
    end type Dirichlet_scalar_value_type

    !>スカラ量を`Dirichlet(p)`
    !>で作成できるようにするためのインタフェース
    interface Dirichlet
        procedure :: Dirichlet_scalar
    end interface

contains
    !>スカラ量を扱う型を構築するためのコンストラクタ．
    function Dirichlet_scalar(val) result(new_val)
        implicit none
        real(real64), intent(in) :: val
            !! スカラ量の値

        type(Dirichlet_scalar_value_type) :: new_val
            !! 派生型にまとめられたスカラ量の値

        new_val%value = val
    end function Dirichlet_scalar
end module grid_uniform_stg_op_cust_bc_vars_scalar_value_Dirichlet
