import { ValueObject } from '../../core/domain/ValueObject'
import { Result } from '../../core/logic/Result'
import { Guard } from '../../core/logic/Guard'

interface Props {
    value: string
}

const MAX_LENGTH = 100

export class RobotTypeModel extends ValueObject<Props> {
    get value(): string {
        return this.props.value
    }

    private constructor(props: Props) {
        super(props)
    }

    public static create(model: string): Result<RobotTypeModel> {
        const guardResult = Guard.againstNullOrUndefined(model, 'model')

        if (!guardResult.succeeded) {
            return Result.fail(guardResult.message)
        } else if (model.length > MAX_LENGTH) {
            return Result.fail(`Model should have no more than ${MAX_LENGTH} characters`)
        }

        return Result.ok(new RobotTypeModel({ value: model.trim() }))
    }
}
