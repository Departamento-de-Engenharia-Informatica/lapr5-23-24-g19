import { ValueObject } from '../../core/domain/ValueObject'
import { Result } from '../../core/logic/Result'

interface Props {
    value: string
}

const regex = /^[a-zA-Z0-9 ]{1,50}$/

export class ElevatorBrand extends ValueObject<Props> {
    get value(): string {
        return this.props.value
    }

    private constructor(props: Props) {
        super(props)
    }

    public static create(brand: string): Result<ElevatorBrand> {
        if (!regex.test(brand)) {
            return Result.fail(
                'Brand must be alphanumeric and have no more than 50 characters',
            )
        }

        return Result.ok(new ElevatorBrand({ value: brand.trim() }))
    }
}
