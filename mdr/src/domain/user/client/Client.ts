import { AggregateRoot } from '../../../core/domain/AggregateRoot'
import { UniqueEntityID } from '../../../core/domain/UniqueEntityID'
import { Guard } from '../../../core/logic/Guard'
import { Result } from '../../../core/logic/Result'
import { Email } from '../email'
import { Name } from '../name'
import { PhoneNumber } from '../phoneNumber'
import { UserPassword } from '../userPassword'
import { VatNumber } from './vatNumber'

interface Props {
    email: Email
    name: Name
    phoneNumber: PhoneNumber
    vatNumber: VatNumber

    password: UserPassword
}

export default class Client extends AggregateRoot<Props> {
    private constructor(props: Props, id?: UniqueEntityID) {
        super(props, id)
    }

    static create(props: Props, id?: UniqueEntityID): Result<Client> {
        const guardResult = Guard.againstNullOrUndefinedBulk([
            { argument: props.email, argumentName: 'Email' },
            { argument: props.name, argumentName: 'Name' },
            { argument: props.phoneNumber, argumentName: 'PhoneNumber' },
            { argument: props.vatNumber, argumentName: 'VATnumber' },
            { argument: props.password, argumentName: 'Password' },
        ])

        if (!guardResult.succeeded) {
            return Result.fail(guardResult.message ?? '')
        }

        return Result.ok(new Client({ ...props }, id))
    }

    get id(): UniqueEntityID {
        return this._id
    }

    get name() {
        return this.props.name
    }

    get email() {
        return this.props.email
    }

    get phoneNumber() {
        return this.props.phoneNumber
    }

    get vatNumber() {
        return this.props.vatNumber
    }
}
