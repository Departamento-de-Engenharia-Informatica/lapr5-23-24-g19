import { ValueObject } from '../../core/domain/ValueObject'
import { Result } from '../../core/logic/Result'
import { Guard } from '../../core/logic/Guard'

interface Props {
    length: number
    width: number
}

// not a fan of this name
export class MaxFloorDimensions extends ValueObject<Props> {
    get length(): number {
        return this.props.length
    }

    get width(): number {
        return this.props.width
    }

    private constructor(props: Props) {
        super(props)
    }

    public static create(length: number, width: number): Result<MaxFloorDimensions> {
        const guardResult = Guard.againstNullOrUndefinedBulk([
            { argument: length, argumentName: 'length' },
            { argument: width, argumentName: 'width' },
        ])

        if (!guardResult.succeeded) {
            return Result.fail(guardResult.message)
        } else if (length <= 0 || width <= 0) {
            return Result.fail(
                'Dimensions must have positive values for length and width',
            )
        } else {
            return Result.ok(new MaxFloorDimensions({ length, width }))
        }
    }

    public fit(dimension: MaxFloorDimensions): boolean {
        return this.length >= dimension.length && this.width >= dimension.width
    }
}
