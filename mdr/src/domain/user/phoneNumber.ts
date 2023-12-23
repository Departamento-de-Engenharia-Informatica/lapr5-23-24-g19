import config from '../../../config'
import { ValueObject } from '../../core/domain/ValueObject'
import { Result } from '../../core/logic/Result'

interface Props {
    value: string
}

const allowedLength = config.phoneNumberLength
const numericRe = /^[0-9]+$/

export class PhoneNumber extends ValueObject<Props> {
    get value() {
        return this.props.value
    }

    private constructor(props: Props) {
        super(props)
    }

    static create(phoneNumber: string | number): Result<PhoneNumber> {
        const num = typeof phoneNumber === 'number' ? phoneNumber.toString() : phoneNumber

        if (num.length != allowedLength || !numericRe.test(num)) {
            console.log('Invalid phone number')
            return Result.fail('Invalid phone number')
        }

        return Result.ok(new PhoneNumber({ value: num }))
    }
}
