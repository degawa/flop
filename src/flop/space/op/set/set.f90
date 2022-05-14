!>デカルト座標系や座標系の各軸に関する手続を提供する．
!>
!>手続には，座標値から軸を構築する手続，
!>軸からデカルト座標系を構築する手続が含まれる．
!>
module space_op_set
    use, intrinsic :: iso_fortran_env
    use :: space_vars_axis
    use :: space_vars_Cartesian
    implicit none
    private
    public :: operator(.set.)

    !>ユーザ定義演算子`.set.`を定義するインタフェース
    interface operator(.set.)
        procedure :: set_coord_to_axis
        procedure :: set_axes_to_Cartesian
    end interface

contains
    !>軸の最小値・最大値を軸に渡し，それらを反映した軸を返す．
    function set_coord_to_axis(axis, coord_val) result(new_axis)
        implicit none
        !&<
        type(axis_type) , intent(in) :: axis
            !! 軸
        real(real64)    , intent(in) :: coord_val(:)
            !! 軸の最小値・最大値
        !&>
        type(axis_type) :: new_axis
            !! 構築される軸

        call new_axis%construct(coord_val)

        return
        if (same_type_as(axis, axis)) continue ! 変数未使用警告の抑制
    end function set_coord_to_axis

    !>軸から構築されたデカルト座標に渡し，
    !>それらを反映したデカルト座標系を返す．
    function set_axes_to_Cartesian(space, cartesian_axes) result(new_space)
        implicit none
        !&<
        type(Cartesian_2d_type) , intent(in) :: space
            !! デカルト座標系型
        type(Cartesian_2d_type) , intent(in) :: cartesian_axes
            !! デカルト座標系を構成する各軸
        !&>
        type(Cartesian_2d_type) :: new_space
            !! 構築されるデカルト座標系

        new_space = cartesian_axes

        return
        if (same_type_as(space, space)) continue ! 変数未使用警告の抑制
    end function set_axes_to_Cartesian
end module space_op_set
