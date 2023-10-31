import { ValueObject } from '../../core/domain/ValueObject'
import { Result } from '../../core/logic/Result'

interface Props {
    value: number
}

export class ElevatorIdentifier extends ValueObject<Props> {
    get value(): number {
        return this.props.value
    }

    private constructor(props: Props) {
        super(props)
    }

    public static create(identifier: number): Result<ElevatorIdentifier> {
        return Result.ok(new ElevatorIdentifier({ value: identifier }))
    }
}
