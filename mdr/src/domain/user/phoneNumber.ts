import config from "../../../config"
import { ValueObject } from "../../core/domain/ValueObject"
import { Result } from "../../core/logic/Result"

interface Props {
    value: number
}

const allowedLength = config.phoneNumberLength

export class PhoneNumber extends ValueObject<Props> {
    get value() {
        return this.props.value
    }

    private constructor(props: Props) {
        super(props)
    }

    static create(phoneNumber: number): Result<PhoneNumber> {
        if (phoneNumber.toString().length != allowedLength) {
            return Result.fail('Invalid phone number')
        }

        return Result.ok(new PhoneNumber({ value: phoneNumber }))
    }
}
