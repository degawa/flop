!>Flopを構成するモジュールを一括して`use`するためのモジュール
module flop
    use :: space_variables
    use :: space_op

    use :: time_variables
    use :: time_op

    use :: discreteTime_variables
    use :: discreteTime_operators

    use :: discretization_op

    use :: grid_uniform_stg_variables
    use :: grid_uniform_stg_operators

    use :: incompressible_variables
    use :: incompressible_op
    public
end module flop
