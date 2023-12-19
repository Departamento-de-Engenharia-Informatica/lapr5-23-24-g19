import { ValueObject } from '../../core/domain/ValueObject'
import { Result } from '../../core/logic/Result'
import { Guard } from '../../core/logic/Guard'

interface Props {
    x: number
    y: number
}

export class Coordinates extends ValueObject<Props> {
    get x(): number {
        return this.props.x
    }

    get y(): number {
        return this.props.y
    }

    private constructor(props: Props) {
        super(props)
    }

    public getValues(): number[] {
        return [this.props.x, this.props.y]
    }

    public static create(x: number, y: number): Result<Coordinates> {
        const guardResult = Guard.againstNullOrUndefinedBulk([
            { argument: x, argumentName: 'x' },
            { argument: y, argumentName: 'y' },
        ])

        if (!guardResult.succeeded) {
            return Result.fail(guardResult.message)
        } else {
            return Result.ok(new Coordinates({ x, y }))
        }
    }
}
