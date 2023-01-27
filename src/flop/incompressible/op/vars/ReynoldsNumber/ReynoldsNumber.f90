!>非圧縮性流れにおける特徴量に関係する派生型や変数，手続を提供する．
!>
!>派生型には，レイノルズ数を取り扱う派生型が含まれる．
!>変数には，レイノルズ数を取り扱う派生型の実体が含まれる．
!>これは，特徴量を管理する型からレイノルズ数を取り出す演算子に渡すための
!>擬似的な型である．
!>
!>@note
!>メインルーチンの中では明確に型宣言されない．
!>演算に使われる他の型の成分として宣言，演算子に渡す値として生成，
!>演算の結果として中間的に生成される．
!>@endnote
!>
!>@note
!>レイノルズ数を取り扱う派生型の実体名は`Reynolds_number`としたかったが，
!>コンストラクタと重複し，コンパイルはできるが参照できない．
!>また，併用することを前提としている`.value.`演算子についても，
!>名前を`.value_of.`とできなかったため，`of_`を変数名に付けることとした．
!>@endnote
!>
!>手続には，レイノルズ数を取り扱う派生型のコンストラクタが含まれる．
!>
module incompressible_op_vars_ReynoldsNumber
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: Reynolds_number

    !>レイノルズ数を取り扱う派生型．
    type, public :: Reynolds_number_type
        real(real64), private :: Re = -1d0
            !! レイノルズ数の値
    contains
        procedure, public, pass :: get_Reynolds_number
        !* レイノルズ数を返却
    end type Reynolds_number_type

    type(Reynolds_number_type), public, parameter :: &
        of_Reynolds_number = Reynolds_number_type()
        !! 演算子にレイノルズ数を認識させるための変数

    !>コンストラクタを`Reynolds_number`と呼ぶためのインタフェース．
    interface Reynolds_number
        procedure :: construct_Reynolds_number
    end interface

contains
    !>レイノルズ数を取り扱う型のコンストラクタ．
    !>レイノルズ数を取り扱う型を返す．
    function construct_Reynolds_number(Re) result(new_Re)
        implicit none

        real(real64), intent(in) :: Re
            !! レイノルズ数の値
        type(Reynolds_number_type) :: new_Re
            !! レイノルズ数

        new_Re%Re = Re
    end function construct_Reynolds_number

    !>レイノルズ数を返す．
    function get_Reynolds_number(this) result(Re)
        implicit none
        class(Reynolds_number_type), intent(in) :: this
            !! 当該実体仮引数
        real(real64) :: Re
            !! 代表速度

        Re = this%Re
    end function get_Reynolds_number
end module incompressible_op_vars_ReynoldsNumber
