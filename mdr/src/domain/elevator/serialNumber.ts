import { ValueObject } from '../../core/domain/ValueObject'
import { Result } from '../../core/logic/Result'

interface Props {
    value: string
}

const regex = /^[a-zA-Z0-9]{1,50}$/

export class ElevatorSerialNumber extends ValueObject<Props> {
    get value(): string {
        return this.props.value
    }

    private constructor(props: Props) {
        super(props)
    }

    public static create(serialNumber: string): Result<ElevatorSerialNumber> {
        if (!regex.test(serialNumber)) {
            return Result.fail(
                'Serial number must be alphanumeric and have no more than 50 characters',
            )
        }

        return Result.ok(new ElevatorSerialNumber({ value: serialNumber }))
    }
}
