import { ValueObject } from '../../core/domain/ValueObject'
import { Result } from '../../core/logic/Result'

interface DescriptionProps {
    value: string
}

const regex = /^[a-zA-Z0-9 ]{1,250}$/

export class ElevatorDescription extends ValueObject<DescriptionProps> {
    get value(): string {
        return this.props.value
    }

    private constructor(props: DescriptionProps) {
        super(props)
    }

    public static create(description: string): Result<ElevatorDescription> {
        if (!regex.test(description)) {
            return Result.fail('Description should have less than 250 characters')
        }

        return Result.ok(new ElevatorDescription({ value: description.trim() }))
    }
}
