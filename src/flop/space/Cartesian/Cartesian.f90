!>デカルト座標系に関する定数や型を提供する．
!>
!>定義される型には，2次元デカルト座標系の情報（各軸の情報）
!>を取り扱う派生型が含まれる．
!>また，各軸の値や量の成分を格納した配列を参照するための定数も定義される．
!>
!>この型を使う限り，各軸の最大，最小値などは，`[x_min, y_min, x_max, ymax]`
!>の順で扱う事になる．
!>
module space_Cartesian
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding
    use :: space_axis, only:axis_type
    implicit none
    private
    public :: x_dir_index, y_dir_index
    public :: x_min_index, x_max_index, &
              y_min_index, y_max_index
    public :: xx_index, xy_index, yx_index, yy_index

    enum, bind(c)
        enumerator :: x_dir_index = 1
            !! デカルト座標系の軸方向成分\(x\)を参照するための配列添字
        enumerator :: y_dir_index
            !! デカルト座標系の軸方向成分\(y\)を参照するための配列添字
    end enum

    enum, bind(c)
        enumerator :: xx_index = 1
            !! デカルト座標系のテンソル量の成分\(xx\)を参照するための配列添字
        enumerator :: xy_index
            !! デカルト座標系のテンソル量の成分\(xy\)を参照するための配列添字
        enumerator :: yx_index
            !! デカルト座標系のテンソル量の成分\(yx\)を参照するための配列添字
        enumerator :: yy_index
            !! デカルト座標系のテンソル量の成分\(yy\)を参照するための配列添字
    end enum

    enum, bind(c)
        enumerator :: x_min_index = 1
            !! デカルト座標系の\(x\)軸の最小値を参照するための配列添字
        enumerator :: y_min_index
            !! デカルト座標系の\(y\)軸の最小値を参照するための配列添字
        enumerator :: x_max_index
            !! デカルト座標系の\(x\)軸の最大値を参照するための配列添字
        enumerator :: y_max_index
            !! デカルト座標系の\(y\)軸の最大値を参照するための配列添字
    end enum

    !>2次元デカルト座標系の各軸の情報を取り扱う派生型．
    type, public :: Cartesian_2d_type
        type(axis_type), private :: x
            !! \(x\)軸
        type(axis_type), private :: y
            !! \(y\)軸
    contains
        !&<
        procedure, public, pass :: set_coordinate_2d_by_array
            !! 配列の値に基づいて座標系の各軸を設定
        procedure, public, pass :: set_coodinate_2d_by_axis
            !! `axis_type`変数に基づいて座標系の各軸を設定
        generic :: construct => set_coordinate_2d_by_array, &
                                set_coodinate_2d_by_axis
        !&>
        procedure, public, pass :: get_coordinate_values => get_coordinate_2d
            !! 座標系の各軸の最小値と最大値を配列で返却<br>
            !! `=[x_min, y_min, x_max, y_max]`
        procedure, public, pass :: get_length => get_length_2d
            !! 座標系の各軸の長さを配列で返却<br>
            !! `=[length_x length_y]`
        procedure, public, pass :: assign
            !! デカルト座標系の値を代入
        generic :: assignment(=) => assign
    end type Cartesian_2d_type

contains
    !>配列の値に基づいて2次元デカルト座標系型変数を設定する．
    subroutine set_coordinate_2d_by_array(this, x_coord_val, y_coord_val)
        implicit none
        !&<
        class(Cartesian_2d_type)  , intent(inout) :: this
            !! 2次元デカルト座標型の当該実体仮引数
        real(real64)              , intent(in)    :: x_coord_val(2)
            !! \(x\)軸の最小値と最大値<br> `[min, max]`の順に格納
        real(real64)              , intent(in)    :: y_coord_val(2)
            !! \(y\)軸の最小値と最大値<br> `[min, max]`の順に格納
        !&>

        this%x = x_coord_val(:)
        this%y = y_coord_val(:)
    end subroutine set_coordinate_2d_by_array

    !>軸の値に基づいて2次元デカルト座標系型変数を設定する．
    subroutine set_coodinate_2d_by_axis(this, x_axis, y_axis)
        implicit none
        !&<
        class(Cartesian_2d_type)    , intent(inout) :: this
            !! 2次元デカルト座標型の当該実体仮引数
        type(axis_type)             , intent(in)    :: x_axis
            !! axis型で表された\(x\)軸
        type(axis_type)             , intent(in)    :: y_axis
            !! axis型で表された\(y\)軸
        !&>

        this%x = x_axis
        this%y = y_axis
    end subroutine set_coodinate_2d_by_axis

    !>2次元デカルト座標系の各軸の座標値を配列で返す．
    function get_coordinate_2d(this) result(coord_vals)
        implicit none
        !&<
        class(Cartesian_2d_type), intent(in)    :: this
            !! 2次元デカルト座標型の当該実体仮引数
        !&>
        real(real64) :: coord_vals(2*2)
            !! 各軸の座標値`=[x_min, y_min, x_max, y_max]`

        real(real64) :: coord_val_x(2), coord_val_y(2)
        coord_val_x = this%x%get_coord_values()
        coord_val_y = this%y%get_coord_values()

        coord_vals(x_min_index) = coord_val_x(1)
        coord_vals(x_max_index) = coord_val_x(2)
        coord_vals(y_min_index) = coord_val_y(1)
        coord_vals(y_max_index) = coord_val_y(2)
    end function get_coordinate_2d

    !>2次元デカルト座標系の各軸の長さを返す．
    function get_length_2d(this) result(lengths)
        implicit none
        !&<
        class(Cartesian_2d_type), intent(in)    :: this
            !! 当該実体仮引数
        !&>
        real(real64) :: lengths(2)
            !! 各軸の長さ`=[length_x length_y]`

        lengths = [this%x%get_length(), &
                   this%y%get_length()]
    end function get_length_2d

    !>2次元デカルト座標系変数を代入する．
    !>単体で呼び出すことはなく，代入演算子`=`をオーバーロード
    !>して利用する．
    subroutine assign(lhs, rhs)
        implicit none
        !&<
        class(Cartesian_2d_type), intent(inout) :: lhs
            !! 代入演算子の左辺値
        class(Cartesian_2d_type), intent(in) :: rhs
            !! 代入演算子の右辺値
        !&>

        lhs%x = rhs%x ! 各軸の代入演算は，`axis_type`で定義
        lhs%y = rhs%y
    end subroutine assign

end module space_Cartesian
