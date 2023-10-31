import { ValueObject } from '../../core/domain/ValueObject'
import { Result } from '../../core/logic/Result'

interface Props {
    value: string
}

const regex = /^[a-zA-Z0-9 ]{1,30}$/

export class RobotNickname extends ValueObject<Props> {
    get value(): string {
        return this.props.value
    }

    private constructor(props: Props) {
        super(props)
    }

    public static create(nickname: string): Result<RobotNickname> {
        if (!regex.test(nickname)) {
            return Result.fail('Robot nickname should have less than 30 characters')
        }

        return Result.ok(new RobotNickname({ value: nickname.trim() }))
    }
}
