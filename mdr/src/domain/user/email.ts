import config from '../../../config'
import { ValueObject } from '../../core/domain/ValueObject'
import { Result } from '../../core/logic/Result'

interface Props {
    value: string
}

// source: https://stackoverflow.com/a/1373724
const nameRegex = /^[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*$/
const validDomains = config.validEmailDomains

export class Email extends ValueObject<Props> {
    get value() {
        return this.props.value
    }

    private constructor(props: Props) {
        super(props)
    }

    static create(email: string): Result<Email> {
        const components = email.split('@')

        if (components.length != 2) {
            return Result.fail('Email can only have one "@" symbol')
        }

        const [name, domain] = components

        if (!validDomains.includes(domain)) {
            return Result.fail('Invalid email domain name')
        } else if (!nameRegex.test(name)) {
            return Result.fail('Invalid email address')
        }

        return Result.ok(new Email({ value: email }))
    }
}
