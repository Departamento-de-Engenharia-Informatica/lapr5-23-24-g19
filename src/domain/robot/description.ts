import { ValueObject } from '../../core/domain/ValueObject'
import { Result } from '../../core/logic/Result'

interface Props {
    value: string
}

const regex = /^[a-zA-Z0-9 ]{1,250}$/

export class RobotDescription extends ValueObject<Props> {
    get value(): string {
        return this.props.value
    }

    private constructor(props: Props) {
        super(props)
    }

    public static create(description: string): Result<RobotDescription> {
        if (!regex.test(description)) {
            return Result.fail('Description should have less than 250 characters')
        }

        return Result.ok(new RobotDescription({ value: description.trim() }))
    }
}
