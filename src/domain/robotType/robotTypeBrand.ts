import { ValueObject } from '../../core/domain/ValueObject'
import { Result } from '../../core/logic/Result'
import { Guard } from '../../core/logic/Guard'

interface Props {
    value: string
}

const MAX_LENGTH = 50

export class RobotTypeBrand extends ValueObject<Props> {
    get value(): string {
        return this.props.value
    }

    private constructor(props: Props) {
        super(props)
    }

    public static create(brand: string): Result<RobotTypeBrand> {
        const guardResult = Guard.againstNullOrUndefined(brand, 'Brand')

        if (!guardResult.succeeded) {
            return Result.fail(guardResult.message)
        } else if (brand.length > MAX_LENGTH) {
            return Result.fail(`Brand should have no more than ${MAX_LENGTH} characters`)
        }

        return Result.ok(new RobotTypeBrand({ value: brand.trim() }))
    }
}
