import { ValueObject } from '../core/domain/ValueObject'
import { Result } from '../core/logic/Result'
import { Guard } from '../core/logic/Guard'

interface DescriptionProps {
    value: string
}

const MAX_LENGTH = 250

export class Description extends ValueObject<DescriptionProps> {
    get value(): string {
        return this.props.value
    }

    private constructor(props: DescriptionProps) {
        super(props)
    }

    public static create(description: string): Result<Description> {
        const guardResult = Guard.againstNullOrUndefined(description, 'description')

        if (!guardResult.succeeded) {
            return Result.fail(guardResult.message)
        } else if (description.length > MAX_LENGTH) {
            return Result.fail('Description should have less than 250 characters')
        }

        return Result.ok(new Description({ value: description }))
    }
}
