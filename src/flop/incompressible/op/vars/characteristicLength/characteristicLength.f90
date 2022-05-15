!>非圧縮性流れにおける特徴量に関係する派生型や変数，手続を提供する．
!>
!>派生型には，代表長さを取り扱う派生型が含まれる．
!>変数には，代表長さを取り扱う派生型の実体が含まれる．
!>これは，特徴量を管理する型から代表長さを取り出す演算子に渡すための
!>擬似的な型である．
!>
!>@note
!>メインルーチンの中では明確に型宣言されない．
!>演算に使われる他の型の成分として宣言，演算子に渡す値として生成，
!>演算の結果として中間的に生成される．
!>@endnote
!>
!>@note
!>代表長さを取り扱う派生型の実体名は`characteristic_length`としたかったが，
!>コンストラクタと重複し，コンパイルはできるが参照できない．
!>また，併用することを前提としている`.value.`演算子についても，
!>名前を`.value_of.`とできなかったため，`of_`を変数名に付けることとした．
!>@endnote
!>
!>手続には，代表長さを取り扱う派生型のコンストラクタが含まれる．
!>
module incompressible_op_vars_characteristicLength
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: characteristic_length

    !>代表長さを取り扱う派生型．
    type, public :: characteristic_length_type
        real(real64), public :: length
            !! 代表長さの値
    end type characteristic_length_type

    type(characteristic_length_type), public :: of_length
        !! 演算子に代表長さを認識させるための変数

    !>コンストラクタを`characteristic_length`と呼ぶためのインタフェース．
    interface characteristic_length
        procedure :: construct_characteristic_length
    end interface

contains
    !>代表長さを取り扱う型のコンストラクタ．
    !>代表長さを取り扱う型を返す．
    function construct_characteristic_length(length) result(new_char_len)
        implicit none

        real(real64), intent(in) :: length
            !! 代表長さの値
        type(characteristic_length_type) :: new_char_len
            !! 代表長さ

        new_char_len%length = length
    end function construct_characteristic_length
end module incompressible_op_vars_characteristicLength
