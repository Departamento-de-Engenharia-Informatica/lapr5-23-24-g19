import { ValueObject } from '../../core/domain/ValueObject'
import { Result } from '../../core/logic/Result'

interface Props {
    value: string
}

const regex = /^[a-zA-Z0-9 ]{1,50}$/

export class ElevatorModel extends ValueObject<Props> {
    get value(): string {
        return this.props.value
    }

    private constructor(props: Props) {
        super(props)
    }

    public static create(model: string): Result<ElevatorModel> {
        if (!regex.test(model)) {
            return Result.fail(
                'Model must be alphanumeric and have no more than 50 characters',
            )
        }

        return Result.ok(new ElevatorModel({ value: model.trim() }))
    }
}
