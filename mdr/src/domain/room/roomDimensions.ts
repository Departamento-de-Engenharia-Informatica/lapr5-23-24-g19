import { ValueObject } from '../../core/domain/ValueObject'
import { Result } from '../../core/logic/Result'
import { Guard } from '../../core/logic/Guard'

interface Props {
    length: number
    width: number
}

export class RoomDimensions extends ValueObject<Props> {
    get length(): number {
        return this.props.length
    }

    get width(): number {
        return this.props.width
    }

    private constructor(props: Props) {
        super(props)
    }

    public static create(length: number, width: number): Result<RoomDimensions> {
        const guardResult = Guard.againstNullOrUndefinedBulk([
            { argument: length, argumentName: 'length' },
            { argument: width, argumentName: 'width' },
        ])

        if (!guardResult.succeeded) {
            return Result.fail(guardResult.message)
        } else if (length < 1 || width < 1) {
            return Result.fail(
                'Dimensions must have at least 1unit value for length and for width',
            )
        } else {
            return Result.ok(new RoomDimensions({ length, width }))
        }
    }
}
