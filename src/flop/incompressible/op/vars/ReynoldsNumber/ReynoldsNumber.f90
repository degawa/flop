!>非圧縮性流れにおける特徴量に関係する派生型や変数，手続を提供する．
!>
!>派生型には，レイノルズ数を取り扱う派生型が含まれる．
!>変数には，レイノルズ数を取り扱う派生型の実体が含まれる．
!>これは，特徴量を管理する型からレイノルズ数を取り出す演算子に渡すための
!>擬似的な型である．
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
module incompressible_op_var_ReynoldsNumber
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: Reynolds_number

    !>レイノルズ数を取り扱う派生型．
    type, public :: Reynolds_number_type
        real(real64), public :: Re
            !! レイノルズ数の値
    end type Reynolds_number_type

    type(Reynolds_number_type), public :: of_Reynolds_number
        !! 演算子にレイノルズ数を認識させるための変数

    !>コンストラクタを`Reynolds_number`と呼ぶためのインタフェース．
    interface Reynolds_number
        procedure :: construct_Reynolds_Number
    end interface

contains
    !>レイノルズ数を取り扱う型のコンストラクタ．
    !>レイノルズ数を取り扱う型を返す．
    function construct_Reynolds_Number(Re) result(new_Re)
        implicit none

        real(real64), intent(in) :: Re
            !! レイノルズ数の値
        type(Reynolds_number_type) :: new_Re
            !! レイノルズ数

        new_Re%Re = Re
    end function construct_Reynolds_Number
end module incompressible_op_var_ReynoldsNumber
