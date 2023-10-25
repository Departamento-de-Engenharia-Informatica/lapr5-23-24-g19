import { ValueObject } from '../../core/domain/ValueObject'
import { Result } from '../../core/logic/Result'
import { Guard } from '../../core/logic/Guard'

interface Props {
    x_axis: number
    y_axis: number
}

export class RoomPositions extends ValueObject<Props> {
    get x_axis(): number {
        return this.props.x_axis
    }

    get y_axis(): number {
        return this.props.y_axis
    }

    private constructor(props: Props) {
        super(props)
    }

    public static create(x_axis: number, y_axis: number): Result<RoomPositions> {
        const guardResult = Guard.againstNullOrUndefinedBulk([
            { argument: x_axis, argumentName: 'x_axis' },
            { argument: y_axis, argumentName: 'y_axis' },
        ])

        if (!guardResult.succeeded) {
            return Result.fail(guardResult.message)
        } else if (!Number.isInteger(x_axis) || !Number.isInteger(y_axis)) {
            return Result.fail('Position values must be integers')
        } else {
            return Result.ok(new RoomPositions({ x_axis, y_axis }))
        }
    }
}
