import { ValueObject } from '../../core/domain/ValueObject'
import { Result } from '../../core/logic/Result'
import { Guard } from '../../core/logic/Guard'

interface FloorNumberProps {
    value: number
}

export class FloorNumber extends ValueObject<FloorNumberProps> {
    get value(): number {
        return this.props.value
    }

    private constructor(props: FloorNumberProps) {
        super(props)
    }

    public static create(floorNumber: number): Result<FloorNumber> {
        const guardResult = Guard.againstNullOrUndefined(floorNumber, 'floorNumber')

        if (!guardResult.succeeded) {
            return Result.fail<FloorNumber>(guardResult.message)
        }

        return Result.ok<FloorNumber>(new FloorNumber({ value: floorNumber }))
    }
}
