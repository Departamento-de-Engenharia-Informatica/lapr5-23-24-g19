import { ValueObject } from '../../core/domain/ValueObject'
import { Result } from '../../core/logic/Result'
import { Guard } from '../../core/logic/Guard'

interface Props {
    value: string
}

const codeRegex = /^[a-zA-Z0-9 ]{1,25}$/

export class RobotTypeCode extends ValueObject<Props> {
    get value(): string {
        return this.props.value
    }

    private constructor(props: Props) {
        super(props)
    }

    public static create(code: string): Result<RobotTypeCode> {
        const guardResult = Guard.againstNullOrUndefined(code, 'code')

        if (!guardResult.succeeded) {
            return Result.fail(guardResult.message)
        }
        code = code.trim()

        if (!codeRegex.test(code)) {
            return Result.fail(
                'Code must contain at most 25 characters, letters and numbers, ' +
                    'possibly with spaces in-between',
            )
        } else {
            return Result.ok(new RobotTypeCode({ value: code }))
        }
    }
}
