import config from '../../../../config'
import { ValueObject } from '../../../core/domain/ValueObject'
import { Result } from '../../../core/logic/Result'

interface Props {
    value: number
}

const allowedLength = config.vatNumberLength

export class VatNumber extends ValueObject<Props> {
    get value() {
        return this.props.value
    }

    private constructor(props: Props) {
        super(props)
    }

    static create(vatNumber: number): Result<VatNumber> {
        if (vatNumber.toString().length != allowedLength) {
            return Result.fail('Invalid phone number')
        }

        return Result.ok(new VatNumber({ value: vatNumber }))
    }
}
