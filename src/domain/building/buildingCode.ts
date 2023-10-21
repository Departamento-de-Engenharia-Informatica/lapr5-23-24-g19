import { ValueObject } from '../../core/domain/ValueObject'
import { Result } from '../../core/logic/Result'
import { Guard } from '../../core/logic/Guard'

interface BuildingCodeProps {
    value: string
}

const codeRegex = /^[A-Za-z ]{1,5}$/

export class BuildingCode extends ValueObject<BuildingCodeProps> {
    get value(): string {
        return this.props.value
    }

    private constructor(props: BuildingCodeProps) {
        super(props)
    }

    public static create(code: string): Result<BuildingCode> {
        const guardResult = Guard.againstNullOrUndefined(code, 'code')

        if (!guardResult.succeeded) {
            return Result.fail<BuildingCode>(guardResult.message)
        }

        if (!codeRegex.test(code.trim())) {
            return Result.fail<BuildingCode>(
                'Code must contain at most 5 characters, letters and numbers, ' + 'possibly with spaces in-between',
            )
        } else {
            return Result.ok<BuildingCode>(new BuildingCode({ value: code }))
        }
    }
}
