!>非圧縮性流れにおける特徴量に関係する派生型や変数，手続を提供する．
!>
!>派生型には，代表速度を取り扱う派生型が含まれる．
!>変数には，代表速度を取り扱う派生型の実体が含まれる．
!>これは，特徴量を管理する型から代表速度を取り出す演算子に渡すための
!>擬似的な型である．
!>
!>@note
!>メインルーチンの中では明確に型宣言されない．
!>演算に使われる他の型の成分として宣言，演算子に渡す値として生成，
!>演算の結果として中間的に生成される．
!>@endnote
!>
!>@note
!>代表速度を取り扱う派生型の実体名は`characteristic_velocity`としたかったが，
!>コンストラクタと重複し，コンパイルはできるが参照できない．
!>また，併用することを前提としている`.value.`演算子についても，
!>名前を`.value_of.`とできなかったため，`of_`を変数名に付けることとした．
!>@endnote
!>
!>手続には，代表速度を取り扱う派生型のコンストラクタが含まれる．
!>
module incompressible_op_vars_characteristicVelocity
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: characteristic_velocity

    !>代表速度を取り扱う派生型．
    type, public :: characteristic_velocity_type
        real(real64), private :: velocity = -1d0
            !! 代表速度の値
    contains
        procedure, public, pass :: get_velocity
    end type characteristic_velocity_type

    type(characteristic_velocity_type), public, parameter :: &
        of_velocity = characteristic_velocity_type()
        !! 演算子に代表速度を認識させるための変数

    !>コンストラクタを`characteristic_velocity`と呼ぶためのインタフェース．
    interface characteristic_velocity
        procedure :: construct_characteristic_velocity
        !* 代表速度を返却
    end interface

contains
    !>代表速度を取り扱う型のコンストラクタ．
    !>代表速度を取り扱う型を返す．
    function construct_characteristic_velocity(velocity) result(new_char_velo)
        implicit none

        real(real64), intent(in) :: velocity
            !! 代表速度の値
        type(characteristic_velocity_type) :: new_char_velo
            !! 代表速度

        new_char_velo%velocity = velocity
    end function construct_characteristic_velocity

    !>代表速度を返す．
    function get_velocity(this) result(velocity)
        implicit none
        class(characteristic_velocity_type), intent(in) :: this
            !! 当該実体仮引数
        real(real64) :: velocity
            !! 代表速度

        velocity = this%velocity
    end function get_velocity
end module incompressible_op_vars_characteristicVelocity
