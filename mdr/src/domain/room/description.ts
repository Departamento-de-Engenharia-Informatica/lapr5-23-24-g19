import { ValueObject } from '../../core/domain/ValueObject'
import { Result } from '../../core/logic/Result'
import { Guard } from '../../core/logic/Guard'

interface DescriptionProps {
    value: string
}

const MAX_LENGTH = 250

export class RoomDescription extends ValueObject<DescriptionProps> {
    get value(): string {
        return this.props.value
    }

    private constructor(props: DescriptionProps) {
        super(props)
    }

    public static create(description: string): Result<RoomDescription> {
        const guardResult = Guard.againstNullOrUndefined(description, 'description')

        if (!guardResult.succeeded) {
            return Result.fail(guardResult.message)
        } else if (description.length > MAX_LENGTH) {
            return Result.fail(
                `Description should have no more than ${MAX_LENGTH} characters`,
            )
        }

        return Result.ok(new RoomDescription({ value: description }))
    }
}
