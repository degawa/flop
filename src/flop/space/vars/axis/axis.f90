!>座標系の各軸に関する定数や派生型を提供する．
!>
!>定義される型には，1次元の軸の情報（最小値と最大値）
!>を取り扱う派生型が含まれる．
!>また，軸の最大値や最小値を配列として扱うので，
!>配列を参照するための定数も定義される．
!>
module space_vars_axis
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding
    implicit none
    private
    public :: axis_min_index, axis_max_index

    enum, bind(c)
        enumerator :: axis_min_index = 1
            !! 軸の最小値を参照するための配列添字
        enumerator :: axis_max_index
            !! 軸の最大値を参照するための配列添字
    end enum

    !>軸の最小値，最大値を取り扱う派生型．
    type, public :: axis_type
        real(real64), private :: min = 0d0
            !! 軸の最小値
        real(real64), private :: max = 0d0
            !! 軸の最大値
    contains
        !&<
        procedure, public, pass :: construct_by_values
        !* 値に基づいて変数を生成
        procedure, public, pass :: construct_by_array
        !* 配列に基づいて変数を生成
        generic :: construct => construct_by_values, &
                                construct_by_array
        !&>

        procedure, public, pass :: set_coord_values
        !* 軸の最小値と最大値を設定
        procedure, public, pass :: get_coord_values
        !* 軸の最小値と最大値を返却
        procedure, public, pass :: get_length
        !* 軸の長さを返却

        procedure, public, pass :: assign_array
        !* 軸の最小値と最大値を持った配列を代入
        procedure, public, pass :: assign_axis
        !* `axis`型の変数を代入
        generic :: assignment(=) => assign_array, assign_axis
    end type axis_type

contains

    !>軸の最小値と最大値を持った配列を軸に代入する．
    !>単体で呼び出すことはなく，代入演算子`=`をオーバーロード
    !>して利用する．
    subroutine assign_array(this, coord_val)
        implicit none
        !&<
        class(axis_type), intent(inout) :: this
            !! 当該実体仮引数
        real(real64)    , intent(in)    :: coord_val(2)
            !! 軸の最小値と最大値<br>
            !! `[min, max]`の順に格納
        !&>
        this%min = coord_val(axis_min_index)
        this%max = coord_val(axis_max_index)
    end subroutine assign_array

    !>`axis`型の変数の値をコピーする．
    !>単体で呼び出すことはなく，代入演算子`=`をオーバーロード
    !>して利用する．
    subroutine assign_axis(this, axis)
        implicit none
        !&<
        class(axis_type), intent(inout) :: this
            !! 当該実体仮引数
        type(axis_type) , intent(in)    :: axis
            !! コピー元の`axis_type`変数
        !&>
        this%min = axis%min
        this%max = axis%max
    end subroutine assign_axis

    !------------------------------------------------------------------!

    !>引数で指定した軸の最小値，最大値に基づいて`axis_type`変数を生成する．
    !>動作は`set_coord_values`と同じであるが，
    !>他の型とインタフェースを統一するために定義される．
    subroutine construct_by_values(this, min_coord_val, max_coord_val)
        implicit none
        !&<
        class(axis_type), intent(inout) :: this
            !! 当該実体仮引数
        real(real64)    , intent(in)    :: min_coord_val
            !! 軸の最小値
        real(real64)    , intent(in)    :: max_coord_val
            !! 軸の最大値
        !&>

        ! `set_coord_values`と動作が同じなので処理を委譲する
        call this%set_coord_values(coord_val=[min_coord_val, max_coord_val])
    end subroutine construct_by_values

    !>引数で指定した，軸の最小値，最大値を持つ配列に基づいて
    !>`axis`型変数を生成する．
    subroutine construct_by_array(this, coord_val)
        implicit none
        class(axis_type), intent(inout) :: this
            !! 当該実体仮引数
        real(real64), intent(in) :: coord_val(2)
            !! 軸の最小値と最大値<br>
            !! `[min, max]`の順に格納

        call this%set_coord_values(coord_val=coord_val)
    end subroutine construct_by_array

    !------------------------------------------------------------------!
    !>軸の最小値と最大値を設定する．
    subroutine set_coord_values(this, coord_val)
        implicit none
        !&<
        class(axis_type), intent(inout) :: this
            !! 当該実体仮引数
        real(real64)    , intent(in)    :: coord_val(2)
            !! 軸の最小値と最大値<br>
            !! `[min, max]`の順に格納
        !&>
        this = coord_val
    end subroutine set_coord_values

    !>軸の最小値と最大値を返す．
    function get_coord_values(this) result(coord_vals)
        implicit none
        !&<
        class(axis_type), intent(in)    :: this
            !! 当該実体仮引数
        real(real64)                    :: coord_vals(2)
            !! 軸の最小値と最大値<br>
            !! `[min, max]`の順に格納<br>
            !! 戻り値
        !&>

        coord_vals(axis_min_index) = this%min
        coord_vals(axis_max_index) = this%max
    end function get_coord_values

    !>軸の長さを返す．
    function get_length(this) result(length)
        implicit none
        !&<
        class(axis_type), intent(in) :: this
            !! 当該実体仮引数
        real(real64) :: length
            !! 軸の長さ<br>
            !! 戻り値
        !&>

        length = this%max - this%min
    end function get_length

end module space_vars_axis
