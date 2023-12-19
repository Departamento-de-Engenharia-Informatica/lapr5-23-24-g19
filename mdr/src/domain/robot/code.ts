import { ValueObject } from '../../core/domain/ValueObject'
import { Result } from '../../core/logic/Result'

interface Props {
    value: string
}

const regex = /^[a-zA-Z0-9]{1,30}$/

export class RobotCode extends ValueObject<Props> {
    get value(): string {
        return this.props.value
    }

    private constructor(props: Props) {
        super(props)
    }

    public static create(code: string): Result<RobotCode> {
        if (!regex.test(code)) {
            return Result.fail(
                `Robot code should be alphanumeric and have less than 30 characters`,
            )
        }

        return Result.ok(new RobotCode({ value: code }))
    }
}
